{-# LANGUAGE DeriveDataTypeable, RecordWildCards, ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-cse #-}
module Main (main) where

import qualified Codec.Archive.Zip.Lazy as Zip

import Control.Applicative
import Control.Exception
import Control.Monad
import Control.Monad.State

import Data.Binary.Get
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as ByteString
import Data.ClassFile.ConstantPool
import Data.Function
import Data.Maybe
import Data.Set (Set)
import qualified Data.Set as Set

import System.Console.CmdArgs
import System.Directory
import System.FilePath
import System.IO.Error hiding (catch)

import Prelude hiding (catch, readFile, zip)

data Judas = Judas
             { classPath :: String
             , internalNames :: [String]
             } deriving (Typeable, Data)

judas :: Judas
judas = Judas { classPath = "." &=
                            explicit &=
                            typ "CLASSPATH" &=
                            name "classpath"
              , internalNames = def &=
                                typ "INTERNAL_NAMES" &=
                                args
              }

main :: IO ()
main = do
  Judas {..} <- cmdArgs judas
  let paths = splitSearchPath classPath
  classPath' <- newClassPath paths
  classFileDependencies classPath' internalNames >>= print . length

type M = StateT S IO

classFileDependencies :: ClassPath -> [String] -> IO [ClassFile]
classFileDependencies classPath' internalNames' =
  fmap catMaybes . flip evalStateT initState . many $ do
    internalName <- pop
    add internalName
    do bytes <- readClass internalName
       mapM_ push (classDependencies bytes)
       return . Just $ ClassFile internalName bytes
       <|>
       return Nothing
  where
    initState = S classPath' Set.empty internalNames'

classDependencies :: ByteString -> [String]
classDependencies = runGet $ do
  getMagic_
  getMinorVersion_
  getMajorVersion_
  constantPool <- getConstantPool
  let xs = foldr f [] constantPool
  return xs
  where
    f (Class a) b = a:b
    f _ b = b
    
getMagic_ :: Get ()
getMagic_ = do
  [0xCA, 0xFE, 0xBA, 0xBE] <- replicateM 4 getWord8
  return ()
    
getMinorVersion_ :: Get ()
getMinorVersion_ = do
  _ <- getWord16be
  return ()
    
getMajorVersion_ :: Get ()
getMajorVersion_ = do
  _ <- getWord16be
  return ()

data S = S { sClassPath :: ClassPath
           , sVisited :: Set String
           , sQueue :: [String]
           }

readClass :: String -> M ByteString
readClass x = do
  S {..} <- get
  lift (readFile sClassPath x)

push :: String -> M ()
push x = do
  s@S {..} <- get
  if Set.member x sVisited
    then return ()
    else put s { sQueue = x:sQueue }

pop :: M String
pop = do
  s@S { sQueue = (x:xs) } <- get
  put s { sQueue = xs }
  return x

add :: String -> M ()
add x = do
  s@S {..} <- get
  put s { sVisited = Set.insert x sVisited }
                  
data ClassFile = ClassFile
                 { internalName :: String
                 , bytes :: ByteString
                 }

newtype ClassPath = ClassPath ([String -> IO ByteString])

newClassPath :: [FilePath] -> IO ClassPath
newClassPath paths = do
  fmap ClassPath . forM paths $ \path -> do
    directoryExists <- doesDirectoryExist path
    if directoryExists
      then return (readInternalNameFromDirectory path)
      else do fileExists <- doesFileExist path
              if fileExists
                then readInternalNameFromZip <$> Zip.openZip path
                else doesNotExistError path
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

readFile :: ClassPath -> String -> IO ByteString
readFile (ClassPath xs) filePath =
  msum' . map ($ filePath) $ xs
  where
    msum' = foldr mplus' mzero'
    mzero' = doesNotExistError filePath
    m `mplus'` n = m `catch` \(_ :: SomeException) -> n

doesNotExistError :: FilePath -> IO a
doesNotExistError filePath =
  ioError (mkIOError doesNotExistErrorType "" Nothing (Just filePath))
