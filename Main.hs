{-# LANGUAGE DeriveDataTypeable, RecordWildCards #-}
{-# OPTIONS_GHC -fno-cse #-}
module Main (main) where

import Control.Monad

import Data.Binary.Get
import qualified Data.ByteString.Lazy as ByteString
import Data.ClassFile.ConstantPool

import System.Console.CmdArgs

data Judas = Judas
             { classPath :: String
             , files :: [FilePath]
             } deriving (Typeable, Data)

judas :: Judas
judas = Judas { classPath = "." &=
                            explicit &=
                            typ "CLASSPATH" &=
                            name "classpath"
              , files = def &= typFile &= args
              }

main :: IO ()
main = do
  Judas {..} <- cmdArgs judas
  mapM_ (ByteString.readFile >=> print. runGet m) files
  where
    m = do
      getMagic_
      getMinorVersion_
      getMajorVersion_
      constantPool <- getConstantPool
      let xs = foldr f [] constantPool
      return xs
      where
        f (Class a) b = a:b
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
