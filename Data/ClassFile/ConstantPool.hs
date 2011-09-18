{-# LANGUAGE
    DoRec
  , FlexibleContexts
  , FlexibleInstances
  , GeneralizedNewtypeDeriving
  , MultiParamTypeClasses
  , StandaloneDeriving #-}
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

import Data.Array
import Data.Binary.Get
import Data.ByteString.UTF8 (toString)
import Data.Int

import Prelude hiding (Integer, length)

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
    
    getCpInfos n = f 1
      where
        f i = do
          (j, x) <- getCpInfo
          let i' = i + j
          ys <- if i' <= n then f i' else return []
          return (y:ys)
    
    getCpInfo = do
      tag <- lift getWord8
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
      nameIndex <- lift getWord16be
      name <- lookupUtf8 nameIndex
      return (1, Class name)
    
    lookupClass classIndex =
      asks (unClass . (!classIndex))
      where
        unClass entry =
          case entry of
            Class name -> name
            _ -> error ("expected Class entry, got: " ++ show entry)
    
    getFieldref = getRef Fieldref
    
    getMethodref = getRef Methodref
    
    getInterfaceMethodref = getRef InterfaceMethodref
    
    getRef mkRef = do
      classIndex <- lift getWord16be
      class' <- lookupClass classIndex
      nameAndTypeIndex <- lift getWord16be
      nameAndType <- lookupNameAndType nameAndTypeIndex
      return (1, mkRef class' nameAndType)
    
    getString = do
      stringIndex <- lift getWord16be
      string <- lookupUtf8 stringIndex
      return (1, String string)
    
    getInteger = do
      x <- lift getWord32be
      return (1, Integer . fromIntegral $ x)
    
    getFloat = undefined
    
    getLong = do
      x <- lift getWord64be
      return (2, Long . fromIntegral $ x)
    
    getDouble = undefined
    
    getNameAndType = do
      nameIndex <- lift getWord16be
      name <- lookupUtf8 nameIndex
      descriptorIndex <- lift getWord16be
      descriptor <- lookupUtf8 descriptorIndex
      return (1, NameAndType (name, descriptor))
    
    lookupNameAndType nameAndTypeIndex =
      asks (unNameAndType . (!nameAndTypeIndex))
      where
        unNameAndType entry =
          case entry of
            NameAndType nameAndType -> nameAndType
            _ -> error ("expected NameAndType, got: " ++ show entry)
    
    getUtf8 = do
      length <- lift getWord16be
      bytes <- lift . getByteString . fromIntegral $ length
      return (1, Utf8 (toString bytes))
    
    lookupUtf8 utf8Index =
      asks (unUtf8 . (!utf8Index))
      where
        unUtf8 entry =
          case entry of
            Utf8 bytes -> bytes
            _ -> error ("expected Utf8, got: " ++ show entry)
