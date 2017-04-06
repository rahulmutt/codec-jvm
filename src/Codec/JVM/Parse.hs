module Codec.JVM.Parse where

import Data.Binary.Get
import Data.Map.Strict (Map)
import Data.ByteString (ByteString)
import Data.ByteString.Lazy (toStrict, readFile)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Set (Set)
import Data.Map as Map
import Data.Word (Word32,Word16)

import qualified Data.List as L
import qualified Data.Text as T
import qualified Data.ByteString.Lazy as BL
import Control.Monad (when,replicateM)

import Codec.JVM.Attr (Attr, putAttr)
import Codec.JVM.Const
import Codec.JVM.ConstPool
import Codec.JVM.Field as F
import Codec.JVM.Internal
import Codec.JVM.Method as M
import Codec.JVM.Types
import qualified Codec.JVM.ConstPool as CP

type ClassName = String
type FieldsMethodsName = [String]

mAGIC :: Word32
mAGIC = 0xCAFEBABE

parseClassFile :: Get (Map ClassName FieldsMethodsName)
parseClassFile = do
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
  superClassIdx <- getWord16be
  let CClass (IClassName isuperClsName) = getConstAt superClassIdx pool
  interfacesCount <- getWord16be
  interfaceNames <- parseInterfaces pool interfacesCount
  fieldsCount <- getWord16be
  fieldInfos <- parseFields pool fieldsCount
  methodsCount <- getWord16be
  methodInfos <- parseMethods pool methodsCount
  return Map.empty

parseInterfaces :: IxConstPool -> Word16 -> Get [Text]
parseInterfaces pool n = replicateM (fromIntegral n) $ parseInterface pool

parseInterface :: IxConstPool -> Get Text
parseInterface pool = do
  tag <- getWord8
  name_index <- getWord16be
  let (CUTF8 interfaceName) = getConstAt name_index pool
  return interfaceName

parseFields :: IxConstPool -> Word16 -> Get [FieldInfo]
parseFields pool n = replicateM (fromIntegral n) $ parseField pool

parseField :: IxConstPool -> Get FieldInfo
parseField cp = do
  access_flags <- getWord16be
  name_index <- getWord16be
  descriptor_index <- getWord16be
  parse_attributes <- parseAttributes
  return $ FieldInfo {
      F.accessFlags = parseAccessFlags access_flags,
      F.name        = parseName cp name_index,
      F.descriptor  = parseDescriptor cp descriptor_index,
      F.attributes  = parse_attributes
    }

parseAccessFlags :: Word16 -> Set AccessFlag
parseAccessFlags = undefined

parseName :: IxConstPool -> Word16 -> UName
parseName pool index = let CUTF8 methodName = getConstAt index pool
                          in UName methodName

parseDescriptor :: IxConstPool -> Word16 -> Desc
parseDescriptor pool index = let CUTF8 descriptor = getConstAt index pool
                                 in Desc descriptor

parseAttributes :: Get [Attr]
parseAttributes = undefined

parseMethods :: IxConstPool -> Word16 -> Get [MethodInfo]
parseMethods pool n = replicateM (fromIntegral n) $ parseMethod pool

parseMethod :: IxConstPool -> Get MethodInfo
parseMethod cp = do
  access_flags <- getWord16be
  name_index <- getWord16be
  descriptor_index <- getWord16be
  parse_attributes <- parseAttributes
  return $ MethodInfo {
      M.accessFlags = parseAccessFlags access_flags,
      M.name        = parseName cp name_index,
      M.descriptor  = parseDescriptor cp descriptor_index
    }
