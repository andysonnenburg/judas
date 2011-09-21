{-# LANGUAGE DeriveDataTypeable, RecordWildCards #-}
{-# OPTIONS_GHC -fno-cse #-}
module Main (main) where

import Control.Exception
import Control.Monad
import Control.Monad.Error ()

import Data.Binary.Get
import qualified Data.ByteString.Lazy as ByteString
import Data.ClassFile.ConstantPool

import Foreign.LibZip

import System.Console.CmdArgs
import System.Directory
import System.FilePath
import System.IO.Error

import Prelude hiding (readFile)

data Judas = Judas
             { classPath :: String
             , files :: [FilePath]
             } deriving (Typeable, Data)

judas :: Judas
judas = Judas { classPath = "." &=
                            explicit &=
                            typ "CLASSPATH" &=
                            name "classpath"
              , files = def &=
                        typ "INTERNAL_NAME" &=
                        args
              }

main :: IO ()
main = do
  Judas {..} <- cmdArgs judas
  -- let classPaths = splitSearchPath classPath
  bracket (openZip "rt.jar") zClose $ \zip ->
    readFile zip "java/util/ArrayList.class" >>=
    print . runGet m . ByteString.fromChunks . (:[])
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
    
    mkReadFile classPaths file =
      msum (map f classPaths)
      where
        f path = do
          dirExists <- doesDirectoryExist path
          if dirExists
            then readFileFromDir path file
            else do fileExists <- doesFileExist path
                    if fileExists
                      then readFileFromJar path file
                      else doesNotExist file
    
    doesNotExist file =
      ioError (mkIOError doesNotExistErrorType "" Nothing (Just file))
    
    readFileFromDir dir file =
      ByteString.readFile (dir </> file)
    
    readFileFromJar jar file = undefined
    
    getMagic_ = do
      [0xCA, 0xFE, 0xBA, 0xBE] <- replicateM 4 getWord8
      return ()
    
    getMinorVersion_ = do
      _ <- getWord16be
      return ()
    
    getMajorVersion_ = do
      _ <- getWord16be
      return ()
