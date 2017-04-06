module Codec.JVM.Parse where

import Data.Binary.Get
import Data.Map.Strict (Map)
import Data.ByteString (ByteString)
import Data.ByteString.Lazy (toStrict, readFile)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Set (Set)
import Data.Map as Map
import Data.Word (Word32,Word16,Word8)

import qualified Data.List as L
import qualified Data.Text as T
import qualified Data.ByteString.Lazy as BL
import Control.Monad (when,replicateM)

import Codec.JVM.Attr
import Codec.JVM.Const
import Codec.JVM.ConstPool
import Codec.JVM.Field as F
import Codec.JVM.Internal
import Codec.JVM.Types
import qualified Codec.JVM.ConstPool as CP

type ClassName = Text
type FieldsMethodsName = [Text]

-- TODO: need to recycle/merge this with Method.hs
data MethodInfo = MethodInfo
  { mi_accessFlags :: Set AccessFlag
  , mi_name :: UName
  , mi_descriptor :: Desc
  , mi_attributes :: [Attr]}
  deriving Show

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
  access_flags <- getAccessFlags ATField
  name_index <- getWord16be
  descriptor_index <- getWord16be
  attributes_count <- getWord16be
  parse_attributes <- parseFieldAttributes cp attributes_count
  return $ FieldInfo {
      F.accessFlags = access_flags,
      F.name        = parseName cp name_index,
      F.descriptor  = parseDescriptor cp descriptor_index,
      F.attributes  = parse_attributes
    }

parseName :: IxConstPool -> Word16 -> UName
parseName pool index = let CUTF8 methodName = getConstAt index pool
                          in UName methodName

parseDescriptor :: IxConstPool -> Word16 -> Desc
parseDescriptor pool index = let CUTF8 descriptor = getConstAt index pool
                                 in Desc descriptor

parseFieldAttributes :: IxConstPool -> Word16 -> Get [Attr]
parseFieldAttributes pool n = replicateM (fromIntegral n) $ parseFieldAttribute pool

parseFieldAttribute :: IxConstPool -> Get Attr
parseFieldAttribute pool = do
  attribute_name_index <- getWord16be
  attribute_length <- getWord32be
  let CUTF8 attributeName = getConstAt attribute_name_index pool
  return $ AConstantValue attributeName

parseMethods :: IxConstPool -> Word16 -> Get [MethodInfo]
parseMethods pool n = replicateM (fromIntegral n) $ parseMethod pool

parseMethod :: IxConstPool -> Get MethodInfo
parseMethod cp = do
  access_flags <- getAccessFlags ATMethod
  name_index <- getWord16be
  descriptor_index <- getWord16be
  attributes_count <- getWord16be
  parse_attributes <- parseMethodAttributes cp attributes_count
  return $ MethodInfo {
      mi_accessFlags = access_flags,
      mi_name        = parseName cp name_index,
      mi_descriptor  = parseDescriptor cp descriptor_index
    }

parseMethodAttributes :: IxConstPool -> Word16 -> Get [Attr]
parseMethodAttributes pool n = replicateM (fromIntegral n) $ parseMethodAttribute pool

parseMethodAttribute :: IxConstPool -> Get Attr
parseMethodAttribute pool = do
  attribute_name_index <- getWord16be
  let (CUTF8 attribute_name) = getConstAt attribute_name_index pool
  attribute_length <- getWord32be
  parameters_count <- getWord8
  parameters <- parseParameters pool parameters_count
  return AMethodParam {
         mp_name = attribute_name,
         mp_parameters = parameters
      }

parseParameters :: IxConstPool -> Word8 -> Get [Parameter]
parseParameters pool n = replicateM (fromIntegral n) $ parseParameter pool

parseParameter :: IxConstPool -> Get Parameter
parseParameter pool = do
  name_index <- getWord16be
  access_flags <- getAccessFlags ATMethodParam
  let CUTF8 parameterName = getConstAt name_index pool
  return (parameterName,access_flags)
