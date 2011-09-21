{-# LANGUAGE NamedFieldPuns #-}
module Foreign.LibZip
       ( Zip
       , openZip
       , zClose
       , readFile
       ) where

#include <zip.h>

import Codec.Archive.Zip.Internal

import Control.Applicative
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
openZip filePath = do
  archive <- withCString filePath $ \path ->
    alloca $ \errorp -> do
      let flags = 0
      archive <- Internal.open path flags errorp
      when (archive == nullPtr) (peek errorp >>= openError)
      return archive
  Zip <$> newForeignPtr Internal.closeFunPtr archive
  where
    openError e = throwIO . ZipException . ("openZip: " ++) $ message
      where
        message = case e of
          #{const ZIP_ER_EXISTS} ->
            "The file specified by " ++ filePath ++
            " exists and ZIP_EXCL is set."
          #{const ZIP_ER_INCONS} ->
            "Inconsistencies were found in the file specified by " ++
            filePath ++ " and ZIP_CHECKCONS was specified."
          #{const ZIP_ER_INVAL} ->
            "The path argument is NULL."
          #{const ZIP_ER_MEMORY} ->
            "Required memory could not be allocated."
          #{const ZIP_ER_NOENT} ->
            "The file specified by " ++ filePath ++
            " does not exist and ZIP_CREATE is not set."
          #{const ZIP_ER_NOZIP} ->
            "The file specified by " ++ filePath ++" is not a zip archive."
          #{const ZIP_ER_OPEN} ->
            "The file specified by " ++ filePath ++ " could not be opened."
          #{const ZIP_ER_READ} ->
            "A read error occurred; see for details."
          #{const ZIP_ER_SEEK} ->
            "The file specified by " ++ filePath ++ " does not allow seeks."
          _ -> "Internal error."

zClose :: Zip -> IO ()
zClose = finalizeForeignPtr . unZip

readFile :: Zip -> FilePath -> IO ByteString
readFile zip name =
  withForeignPtr (unZip zip) $ \archive -> do
    Internal.ZipStat { index, size } <- withCString name $ \fname ->
      alloca $ \sb -> do
        let flags = 0
        result <- Internal.stat archive fname flags sb
        when (result == -1) (strerror "readFile: " archive)
        peek sb
    let flags = 0
    file <- Internal.fopenIndex archive index flags
    when (file == nullPtr) (strerror "readFile: " archive)
    let nbytes = fromIntegral size
    (bracketOnError (mallocBytes (fromIntegral size)) free $ \buf -> do
      result <- Internal.fread file buf nbytes
      when (result == -1) (fileStrerror "readFile: " file)
      when (result /= nbytes) (fail "internal error")
      unsafePackCStringFinalizer buf (fromIntegral result) (free buf))
      `finally`
      Internal.fclose file

strerror :: String -> Ptr Internal.Zip -> IO a
strerror prefix =
  Internal.strerror >=>
  peekCString >=>
  throwIO . ZipException . (prefix ++)

fileStrerror :: String -> Ptr Internal.ZipFile -> IO a
fileStrerror prefix =
  Internal.fileStrerror >=>
  peekCString >=>
  throwIO . ZipException . (prefix ++)