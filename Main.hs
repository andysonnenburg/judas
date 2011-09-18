{-# LANGUAGE DeriveDataTypeable, RecordWildCards #-}
module Main (main) where

import Control.Monad

import Data.Binary.Get
import qualified Data.ByteString.Lazy as ByteString
import Data.ClassFile.ConstantPool

import System.Console.CmdArgs hiding (name)

data Judas = Judas
             { files :: [FilePath]
             } deriving (Typeable, Data)

judas :: Judas
judas = Judas { files = def &= typFile &= args }

main :: IO ()
main = do
  Judas {..} <- cmdArgs judas
  mapM_ (ByteString.readFile >=> print . runGet m) files
  where
    m = do
      getMagic_
      getMinorVersion_
      getMajorVersion_
      constantPool <- getConstantPool
      let xs = foldr f [] constantPool
      return xs
      where
        f (Class name) b = name:b
        f _ b = b
      
    
    getMagic_ = do
      [0xCA, 0xFE, 0xBA, 0xBE] <- replicateM 4 getWord8
      return ()
    
    getMinorVersion_ = do
      _ <- getWord16be
      return ()
    
    getMajorVersion_ = do
      _ <- getWord16be
      return ()
