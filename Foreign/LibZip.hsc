{-# LANGUAGE NamedFieldPuns #-}
module Foreign.LibZip
       ( Zip
       , openZip
       , zClose
       , readFile
       ) where

#include <zip.h>

import Control.Exception
import Control.Monad

import Data.ByteString (ByteString)
import Data.ByteString.Unsafe (unsafePackCStringFinalizer)

import Foreign
import Foreign.C.String
import Foreign.LibZip.Internal (index, size)
import qualified Foreign.LibZip.Internal as Internal

import Prelude hiding (readFile, zip)

newtype Zip = Zip { unZip :: ForeignPtr Internal.Zip }

openZip :: FilePath -> IO Zip
openZip = flip withCString f >=> fmap Zip . newForeignPtr Internal.closeFunPtr
  where
    f path =
      alloca $ \errorp -> do
        zip <- Internal.open path flags errorp
        when (zip == nullPtr) (peek errorp >>= openError)
        return zip
      where
        flags = 0
        openError e = fail message
          where
            message = case e of
              #{const ZIP_ER_EXISTS} ->
                "The file specified by path exists and ZIP_EXCL is set."
              #{const ZIP_ER_INCONS} ->
                "Inconsistencies were found in the file specified by path " ++
                "and ZIP_CHECKCONS was specified."
              #{const ZIP_ER_INVAL} ->
                "The path argument is NULL."
              #{const ZIP_ER_MEMORY} ->
                "Required memory could not be allocated."
              #{const ZIP_ER_NOENT} ->
                "The file specified by path does not exist and ZIP_CREATE " ++
                "is not set."
              #{const ZIP_ER_NOZIP} ->
                "The file specified by path is not a zip archive."
              #{const ZIP_ER_OPEN} ->
                "The file specified by path could not be opened."
              #{const ZIP_ER_READ} ->
                "A read error occurred; see for details."
              #{const ZIP_ER_SEEK} ->
                "The file specified by path does not allow seeks."

zClose :: Zip -> IO ()
zClose = finalizeForeignPtr . unZip

readFile :: Zip -> FilePath -> IO ByteString
readFile zip name =
  withForeignPtr (unZip zip) $ \archive -> do
    Internal.ZipStat { index, size } <- withCString name $ \fname ->
      alloca $ \sb -> do
        let flags = 0
        result <- Internal.stat archive fname flags sb
        when (result == -1) (strerror archive)
        peek sb
    let flags = 0
    file <- Internal.fopenIndex archive index flags
    when (file == nullPtr) (strerror archive)
    let nbytes = fromIntegral size
    (bracketOnError (mallocBytes (fromIntegral nbytes)) free $ \buf -> do
      result <- Internal.fread file buf nbytes
      when (result == -1) (fileStrerror file)
      when (result /= nbytes) (fail "internal error")
      unsafePackCStringFinalizer buf (fromIntegral result) (free buf))
      `finally`
      Internal.fclose file

strerror :: Ptr Internal.Zip -> IO a
strerror = Internal.strerror >=> peekCString >=> fail

fileStrerror :: Ptr Internal.ZipFile -> IO a
fileStrerror = Internal.fileStrerror >=> peekCString >=> fail