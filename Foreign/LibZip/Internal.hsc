{-# LANGUAGE EmptyDataDecls, ForeignFunctionInterface #-}
module Foreign.LibZip.Internal
       ( Zip
       , ZipFile
       , ZipStat (..)
       , zip_open
       , zip_name_locate
       , zip_fopen
       , zip_fopen_index
       , zip_fread
       , zip_fclose
       , zip_close
       , zip_stat
       , zip_stat_index
       ) where

#include <zip.h>

#let alignment t = "%lu", (unsigned long) offsetof(struct { char x__; t (y__); }, y__)

import Foreign
import Foreign.C.Types
import Foreign.C.String

data Zip
data ZipFile
data ZipStat = ZipStat
               { name :: CString
               , index :: CInt
               , crc :: CUInt
               , size :: CUInt
               , mtime :: CTime
               , comp_size :: CUInt
               , comp_method :: CUShort
               , encryption_method :: CUShort
               }

instance Storable ZipStat where
  sizeOf _ = #size zip_stat
  alignment _ = #aligment zip_stat
  peek ptr =
    ZipStat <$>
    #{peek struct zip_stat, name} ptr <*>
    #{peek struct zip_stat, index} ptr <*>
    #{peek struct zip_stat, crc} ptr <*>
    #{peek struct zip_stat, size} ptr <*>
    #{peek struct zip_stat, mtime} ptr <*>
    #{peek struct zip_stat, comp_size} ptr <*>
    #{peek struct zip_stat, comp_method} ptr <*>
    #{peek struct zip_stat, encryption_method} ptr
  poke ptr ZipStat {..} = do
    #{poke struct zip_stat, name} ptr name
    #{poke struct zip_stat, index} ptr index
    #{poke struct zip_stat, crc} ptr crc
    #{poke struct zip_stat, size} ptr size
    #{poke struct zip_stat, mtime} ptr mtime
    #{poke struct zip_stat, comp_size} ptr comp_size
    #{poke struct zip_stat, comp_method} ptr comp_method
    #{poke struct zip_stat, encryption_method} ptr encryption_method

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

foreign import ccall "zip_stat_index"
  zip_stat_index :: Ptr Zip -> CInt -> CInt -> Ptr ZipStat -> IO CInt