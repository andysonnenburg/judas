{-# LANGUAGE EmptyDataDecls, ForeignFunctionInterface, RecordWildCards #-}
module Foreign.LibZip.Internal
       ( Zip
       , ZipFile
       , ZipStat (..)
       , open
       , nameLocate
       , fopen
       , fopenIndex
       , fread
       , fclose
       , close
       , closeFunPtr
       , stat
       , statIndex
       , strerror
       , fileStrerror
       ) where

#include <zip.h>

#let alignment t = "%lu", (unsigned long) offsetof(struct { char x__; t (y__); }, y__)

import Control.Applicative

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
  sizeOf _ = #{size struct zip_stat}
  alignment _ = #{alignment struct zip_stat}
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

foreign import ccall unsafe "zip_open"
  open :: CString -> CInt -> Ptr CInt -> IO (Ptr Zip)

foreign import ccall unsafe "zip_name_locate"
  nameLocate :: Ptr Zip -> CString -> CInt -> IO CInt

foreign import ccall unsafe "zip_fopen"
  fopen :: Ptr Zip -> CString -> CInt -> IO (Ptr ZipFile)

foreign import ccall unsafe "zip_fopen_index"
  fopenIndex :: Ptr Zip -> CInt -> CInt -> IO (Ptr ZipFile)

foreign import ccall unsafe "zip_fread"
  fread :: Ptr ZipFile -> Ptr Word8 -> CInt -> IO CInt

foreign import ccall unsafe "zip_fclose"
  fclose :: Ptr ZipFile -> IO CInt

foreign import ccall unsafe "zip_close"
  close :: Ptr Zip -> IO CInt

foreign import ccall unsafe "&zip_close"
  closeFunPtr :: FunPtr (Ptr Zip -> IO ())

foreign import ccall unsafe "zip_stat"
  stat :: Ptr Zip -> CString -> CInt -> Ptr ZipStat -> IO CInt

foreign import ccall unsafe "zip_stat_index"
  statIndex :: Ptr Zip -> CInt -> CInt -> Ptr ZipStat -> IO CInt

foreign import ccall unsafe "zip_strerror"
  strerror :: Ptr Zip -> IO CString

foreign import ccall unsafe "zip_file_strerror"
  fileStrerror :: Ptr ZipFile -> IO CString