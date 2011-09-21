{-# LANGUAGE DeriveDataTypeable, RecordWildCards, ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-cse #-}
module Main (main) where

import qualified Codec.Archive.Zip.Lazy as Zip

import Control.Applicative
import Control.Exception
import Control.Monad

import Data.Binary.Get
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as ByteString
import Data.ClassFile.ConstantPool
import Data.Function

import System.Console.CmdArgs
import System.Directory
import System.FilePath
import System.IO.Error hiding (catch)

import Prelude hiding (catch, readFile, zip)

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
                        typ "INTERNAL_NAMES" &=
                        args
              }

main :: IO ()
main = do
  Judas {..} <- cmdArgs judas
  let paths = splitSearchPath classPath
  classPath' <- newClassPath paths
  forM_ files $
    readFile classPath' >=> print . runGet m
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

data ClassFile = ClassFile
                 { internalName :: String
                 , bytes :: ByteString
                 }

instance Eq ClassFile where
  (==) = (==) `on` internalName

instance Ord ClassFile where
  compare = compare `on` internalName

newtype ClassPath = ClassPath ([FilePath -> IO ByteString])

newClassPath :: [FilePath] -> IO ClassPath
newClassPath paths = do
  xs <- forM paths $ \path -> do
    directoryExists <- doesDirectoryExist path
    if directoryExists
      then return (readInternalNameFromDirectory path)
      else do fileExists <- doesFileExist path
              if fileExists
                then readInternalNameFromZip <$> Zip.openZip path
                else doesNotExistError path
  return (ClassPath xs)
  where
    readInternalNameFromDirectory directory internalName =
      ByteString.readFile (directory </> file)
      where
        file = fromInternalName internalName
    
    readInternalNameFromZip zip internalName =
      Zip.readFile zip file
      where
        file = fromInternalName internalName
    
    fromInternalName = (`addExtension` ".class")

readFile :: ClassPath -> FilePath -> IO ByteString
readFile (ClassPath xs) filePath =
  msum' . map ($ filePath) $ xs
  where
    msum' = foldr mplus' mzero'
    mzero' = doesNotExistError filePath
    m `mplus'` n = m `catch` \(_ :: SomeException) -> n

doesNotExistError :: FilePath -> IO a
doesNotExistError filePath =
  ioError (mkIOError doesNotExistErrorType "" Nothing (Just filePath))
