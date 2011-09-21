module Codec.Archive.Zip.Lazy
       ( module Codec.Archive.Zip
       , readFile
       ) where

import Codec.Archive.Zip hiding (readFile)
import qualified Codec.Archive.Zip as Zip

import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as ByteString

import Prelude hiding (readFile, zip)

readFile :: Zip -> FilePath -> IO ByteString
readFile zip = fmap (ByteString.fromChunks . (:[])) . Zip.readFile zip