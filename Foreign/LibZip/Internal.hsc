{-# LANGUAGE EmptyDataDecls, ForeignFunctionInterface #-}
module Foreign.LibZip.Internal
       ( Zip
       , ZipFile
       ) where

-- #include <zip.h>

import Foreign
import Foreign.C.Types
import Foreign.C.String

data Zip
data ZipFile
data ZipStat

foreign import ccall "zip_open"
  zip_open :: CString -> CInt -> Ptr CInt -> IO (Ptr Zip)

foreign import ccall "zip_name_locate"
  zip_name_locate :: Ptr Zip -> CString -> CInt -> IO CInt

foreign import ccall "zip_fopen"
  zip_fopen :: Ptr Zip -> CString -> CInt -> IO (Ptr ZipFile)

foreign import ccall "zip_fopen_index"
  zip_fopen_index :: Ptr Zip -> CString -> CInt -> IO (Ptr ZipFile)

foreign import ccall "zip_fread"
  zip_fread :: Ptr ZipFile -> Ptr Word8 -> CInt -> IO CInt

foreign import ccall "zip_fclose"
  zip_fclose :: Ptr ZipFile -> IO CInt

foreign import ccall "zip_close"
  zip_close :: Ptr Zip -> IO CInt

foreign import ccall "zip_stat"
  zip_stat :: Ptr Zip -> CString -> CInt -> Ptr ZipStat -> IO CInt