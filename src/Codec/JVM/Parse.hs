module Codec.JVM.Parse where

import Data.Binary.Get
import Data.Map.Strict (Map)
import Data.ByteString (ByteString)
import Data.ByteString.Lazy (toStrict, readFile)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Set (Set)
import Data.Word (Word32,Word16)

import qualified Data.List as L
import qualified Data.Text as T
import qualified Data.ByteString.Lazy as BL
import Control.Monad (when,forM_)

import Codec.JVM.Attr (Attr, putAttr)
import Codec.JVM.Const
import Codec.JVM.ConstPool
import Codec.JVM.Field
import Codec.JVM.Internal
import Codec.JVM.Method (MethodInfo, putMethodInfo)
import Codec.JVM.Types
import qualified Codec.JVM.ConstPool as CP

mAGIC :: Word32
mAGIC = 0xCAFEBABE

getClassName :: Get Text
getClassName = do
  magic <- getWord32be
  when (magic /= mAGIC) $
    fail $ "Invalid .class file MAGIC value: " ++ show magic
  minorVersion <- getWord16be
  majorVersion <- getWord16be
  poolSize <- getWord16be
  pool <- getConstPool $ fromIntegral $ poolSize - 1
  afs <- getAccessFlags ATClass
  classIdx <- getWord16be
  let CClass (IClassName iclsName) = getConstAt classIdx pool
  return iclsName

{-

ClassFile {
    u4             magic;
    u2             minor_version;
    u2             major_version;
    u2             constant_pool_count;
    cp_info        constant_pool[constant_pool_count-1];
    u2             access_flags;
    u2             this_class;
    u2             super_class;
    u2             interfaces_count;
    u2             interfaces[interfaces_count];
    u2             fields_count;
    field_info     fields[fields_count];
    u2             methods_count;
    method_info    methods[methods_count];
    u2             attributes_count;
    attribute_info attributes[attributes_count];
}


-}
parseClassFile :: Get ()
parseClassFile = do
  magic <- getWord32be
  minorVersion <- getWord16be
  majorVersion <- getWord16be
  poolSize <- getWord16be
  pool <- getConstPool $ fromIntegral $ poolSize - 1
  afs <- getAccessFlags ATClass
  classIdx <- getWord16be
  let CClass (IClassName iclsName) = getConstAt classIdx pool
  superClassIdx <- getWord16be
  let CClass (IClassName isuperClsName) = getConstAt superClassIdx pool
  interfacesCount <- getWord16be
  getWord16be
  fieldsCount <- getWord16be

  methodsCount <- getWord16be

  attributesCount <- getWord16be

  return ()
  --let CClass (IClassName iclsName) = getConstAt classIdx pool
  --return iclsName

-- parseFields :: IxConstPool -> Word16 -> Get [FieldInfo]
-- parseFields pool n = forM_ [1..n] $ parseField pool

parseField :: IxConstPool -> Get FieldInfo
parseField cp = do
  access_flags <- getWord16be
  name_index <- getWord16be
  descriptor_index <- getWord16be
  parse_attributes <- parseAttributes
  return $ FieldInfo {
      accessFlags = parseAccessFlags access_flags,
      name        = parseName cp name_index,
      descriptor  = parseDescriptor cp descriptor_index,
      attributes  = parse_attributes
    }

parseAccessFlags :: Word16 -> Set AccessFlag
parseAccessFlags = undefined

parseName :: IxConstPool -> Word16 -> UName
parseName = undefined

parseDescriptor :: IxConstPool -> Word16 -> Desc
parseDescriptor = undefined

parseAttributes :: Get [Attr]
parseAttributes = undefined
