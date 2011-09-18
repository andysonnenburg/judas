{-# LANGUAGE DoRec #-}
module Data.ClassFile.ConstantPool
       ( ConstantPool
       , ConstantPoolEntry (..)
       , Name
       , Class
       , NameAndType
       , Descriptor
       , Integer
       , Long
       , getConstantPool
       ) where

import Control.Monad.Reader
import Control.Monad.State

import Data.Array hiding (index)
import Data.Binary.Get
import Data.Binary.IEEE754
import Data.ByteString.UTF8 (toString)
import Data.Int

import Prelude hiding (Integer, error, length)
import qualified Prelude

type ConstantPool = [ConstantPoolEntry]

data ConstantPoolEntry = Class Name
                       | Fieldref Class NameAndType
                       | Methodref Class NameAndType
                       | InterfaceMethodref Class NameAndType
                       | String String
                       | Integer Integer
                       | Float Float
                       | Long Long
                       | Double Double
                       | NameAndType NameAndType
                       | Utf8 String deriving Show

type Name = String

type Class = String

type NameAndType = (Name, Descriptor)

type Descriptor = String

type Integer = Int32

type Long = Int64

getConstantPool :: Get ConstantPool
getConstantPool = do
  constantPoolCount <- getConstantPoolCount
  let n = constantPoolCount - 1
  rec a <- runReaderT (getCpInfos n) (array (1, n) a)
  return . map snd $ a
  where
    getConstantPoolCount = getWord16be
    
    getCpInfos n = evalStateT m 1
      where
        m = do
          i <- get
          if i > n
            then return []
            else do x <- getCpInfo
                    xs <- m
                    return (x:xs)
    
    getCpInfo = do
      tag <- lift' getWord8
      case tag of
        7 -> getClass
        9 -> getFieldref
        10 -> getMethodref
        11 -> getInterfaceMethodref
        8 -> getString
        3 -> getInteger
        4 -> getFloat
        5 -> getLong
        6 -> getDouble
        12 -> getNameAndType
        1 -> getUtf8
        _ -> fail ("unexpected tag: " ++ show tag)
    
    getClass = do
      nameIndex <- lift' getWord16be
      name <- lookupUtf8 nameIndex
      return' (Class name)
    
    lookupClass classIndex =
      asks (unClass . (!classIndex))
      where
        unClass entry =
          case entry of
            Class name -> name
            _ -> error "Class" entry classIndex
    
    getFieldref = getRef Fieldref
    
    getMethodref = getRef Methodref
    
    getInterfaceMethodref = getRef InterfaceMethodref
    
    getRef mkRef = do
      classIndex <- lift' getWord16be
      class' <- lookupClass classIndex
      nameAndTypeIndex <- lift' getWord16be
      nameAndType <- lookupNameAndType nameAndTypeIndex
      return' (mkRef class' nameAndType)
    
    getString = do
      stringIndex <- lift' getWord16be
      string <- lookupUtf8 stringIndex
      return' (String string)
    
    getInteger = do
      x <- lift' getWord32be
      return' . Integer . fromIntegral $ x
    
    getFloat = do
      x <- lift' getFloat32be
      return' (Float x)
    
    getLong = do
      x <- lift' getWord64be
      return'' . Long . fromIntegral $ x
    
    getDouble = do
      x <- lift' getFloat64be
      return'' (Double x)
    
    getNameAndType = do
      nameIndex <- lift' getWord16be
      name <- lookupUtf8 nameIndex
      descriptorIndex <- lift' getWord16be
      descriptor <- lookupUtf8 descriptorIndex
      return' (NameAndType (name, descriptor))
    
    lookupNameAndType nameAndTypeIndex =
      asks (unNameAndType . (!nameAndTypeIndex))
      where
        unNameAndType entry =
          case entry of
            NameAndType nameAndType -> nameAndType
            _ -> error "NameAndType" entry nameAndTypeIndex
    
    getUtf8 = do
      length <- lift' getWord16be
      bytes <- lift' . getByteString . fromIntegral $ length
      return' (Utf8 (toString bytes))
    
    lookupUtf8 utf8Index =
      asks (unUtf8 . (!utf8Index))
      where
        unUtf8 entry =
          case entry of
            Utf8 bytes -> bytes
            _ -> error "Utf8" entry utf8Index
    
    error expected actual index = Prelude.error message
      where
        message = showString "expected " . shows expected .
                  showString ", got " . shows actual .
                  showString " at " . shows index $ ""
    
    return' x = do
      i <- get
      put (i + 1)
      return (i, x)
    
    return'' x = do
      i <- get
      put (i + 2)
      return (i, x)
    
    lift' = lift . lift
