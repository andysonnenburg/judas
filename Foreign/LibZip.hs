module Foreign.LibZip
       ( Zip
       , openZip
       , zClose
       , readFile
       ) where

import Data.ByteString (ByteString)

newtype Zip = Zip { unZip :: ForeignPtr Internal.Zip }

openZip :: FilePath -> IO Zip
openZip = Zip . newForeignPtr Internal.closeFunPtr . flip withCString f
  where
    f path =
      alloca $ \errorp -> do
        zip <- Internal.open path flags errorp
        if zip == nullPtr
          then peek errorp >>= openError
          else return zip
      where
        flags = 0
        openError e = undefined

zClose :: Zip -> IO ()
zClose = undefined

readFile :: Zip -> FilePath -> IO ByteString
readFile = undefined