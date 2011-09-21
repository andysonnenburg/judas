{-# LANGUAGE DeriveDataTypeable #-}
module Codec.Archive.Zip.Internal
       ( ZipException (..)
       ) where

import Control.Exception

import Data.Typeable

newtype ZipException = ZipException String deriving (Typeable, Show)

instance Exception ZipException