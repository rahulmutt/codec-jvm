{-# LANGUAGE OverloadedStrings #-}
module Codec.JVM.Parse where

import Data.Binary.Get
import Data.Map.Strict (Map)
import Data.ByteString (ByteString)
import Data.ByteString.Lazy (toStrict, readFile)
import Data.Maybe (fromMaybe,fromJust,catMaybes)
import Data.Text (Text)
import Data.Set (Set)
import Data.Map as Map
import Data.Char
import Data.Word (Word32,Word16,Word8)

import qualified Data.List as L
import qualified Data.Text as T
import qualified Data.ByteString.Lazy as BL
import Control.Monad (when,replicateM)
import Control.Applicative ((<|>), some)

import Codec.JVM.Attr
import Codec.JVM.Const
import Codec.JVM.ConstPool
import Codec.JVM.Field as F
import Codec.JVM.Internal
import Codec.JVM.Types
import qualified Codec.JVM.ConstPool as CP
import Text.ParserCombinators.ReadP

-- TODO: abstract out the replicateM bit

-- TODO: need to recycle/merge this with Method.hs
data MethodInfo = MethodInfo
  { mi_accessFlags :: Set AccessFlag
  , mi_name :: UName
  , mi_descriptor :: Desc
  , mi_attributes :: [Attr]}
  deriving Show

data Info = Info
  {  interfaces  :: [InterfaceName]
   , fieldInfos  :: [FieldInfo]
   , methodInfos :: [MethodInfo]
   , classAttributes :: [Attr]}
   deriving Show

mAGIC :: Word32
mAGIC = 0xCAFEBABE

parseClassFileHeaders :: Get (ClassName,SuperClassName,[InterfaceName])
parseClassFileHeaders = do
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
  interfaceNames <- parseInterfaces pool interfacesCount -- :: [InterfaceName]
  return (iclsName,isuperClsName,interfaceNames)

parseClassFile :: Get (ClassName,Info)
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
  interfaceNames <- parseInterfaces pool interfacesCount -- :: [InterfaceName]
  (iclsName,isuperClsName,interfaceNames) <- parseClassFileHeaders
  fieldsCount <- getWord16be
  fieldInfos <- parseFields pool fieldsCount -- :: [FieldInfo]
  methodsCount <- getWord16be
  methodInfos <- parseMethods pool methodsCount -- :: [MethodInfo]
  attributesCount <- getWord16be
  parseAttributes <- parseClassAttributes pool attributesCount
  return (iclsName,
    Info { interfaces  = interfaceNames
         ,fieldInfos  = fieldInfos
         ,methodInfos = methodInfos
         ,classAttributes = parseAttributes})

parseClassAttributes :: IxConstPool -> Word16 -> Get [Attr]
parseClassAttributes pool n = fmap catMaybes
  $ replicateM (fromIntegral n) $ parseClassAttribute pool

parseClassAttribute :: IxConstPool -> Get (Maybe Attr)
parseClassAttribute pool = do
  attribute_name_index <- getWord16be
  attribute_length     <- getWord32be
  let CUTF8 attributeName = getConstAt attribute_name_index pool
  case attributeName of
    "Signature" -> fmap Just $ parseClassSignature pool
    _           -> skip (fromIntegral attribute_length) >> return Nothing

parseInterfaces :: IxConstPool -> Word16 -> Get [InterfaceName]
parseInterfaces pool n = replicateM (fromIntegral n) $ parseInterface pool

parseInterface :: IxConstPool -> Get InterfaceName
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
parseFieldAttributes pool n = fmap catMaybes $
  replicateM (fromIntegral n) $ parseFieldAttribute pool

parseFieldAttribute :: IxConstPool -> Get (Maybe Attr)
parseFieldAttribute pool = do
  attribute_name_index <- getWord16be
  attribute_length     <- getWord32be
  let CUTF8 attributeName = getConstAt attribute_name_index pool
  case attributeName of
    "ConstantValue" -> fmap Just $ parseConstantValue pool
    "Signature"     -> fmap Just $ parseFieldSignature pool
    _               -> skip (fromIntegral attribute_length) >> return Nothing

showText :: Show a => a -> Text
showText = T.pack . show

munch1Text :: (Char -> Bool) -> ReadP Text
munch1Text pred = fmap T.pack $ munch1 pred

parseConstantValue :: IxConstPool -> Get Attr
parseConstantValue pool = do
  constant_value_index <- getWord16be
  let (CValue x) = getConstAt constant_value_index pool
  case x of
    CString s  -> return $ AConstantValue s
    CInteger i -> return $ AConstantValue $ showText i
    CLong l    -> return $ AConstantValue $ showText l
    CFloat f   -> return $ AConstantValue $ showText f
    CDouble d  -> return $ AConstantValue $ showText d

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
      mi_descriptor  = parseDescriptor cp descriptor_index,
      mi_attributes  = parse_attributes
    }

parseMethodAttributes :: IxConstPool -> Word16 -> Get [Attr]
parseMethodAttributes pool n = fmap catMaybes
  $ replicateM (fromIntegral n) $ parseMethodAttribute pool

parseMethodAttribute :: IxConstPool -> Get (Maybe Attr)
parseMethodAttribute pool = do
  attribute_name_index <- getWord16be
  attribute_length     <- getWord32be
  let (CUTF8 attribute_name) = getConstAt attribute_name_index pool
  case attribute_name of
    "Signature" -> fmap Just $ parseMethodSignature pool
    "MethodParameters" -> fmap Just $ parseMethodParameters pool
    _ -> skip (fromIntegral attribute_length) >> return Nothing

parseMethodParameters :: IxConstPool -> Get Attr
parseMethodParameters pool = do
  parameters_count <- getWord8
  parameters <- parseMParameters pool parameters_count
  return $ AMethodParam parameters

parseMParameters :: IxConstPool -> Word8 -> Get [MParameter]
parseMParameters pool n = replicateM (fromIntegral n) $ parseMethodParameter pool

parseMethodParameter :: IxConstPool -> Get MParameter
parseMethodParameter pool = do
  name_index <- getWord16be
  access_flags <- getAccessFlags ATMethodParam
  let CUTF8 parameterName = getConstAt name_index pool
  return (parameterName,access_flags)

-- Parsing Signatures
-- Examples:
-- (Ljava/lang/String;II)
-- (TT;Ljava/util/List<TU;>;Ljava/util/ArrayList<TE;>;)
-- (TT;Ljava/util/List<-TX;>;Ljava/util/ArrayList<+TY;>;)
-- (Ljava/lang/Class<*>;)
-- ? extends List<T>
-- +Ljava/util/List<TT;>
-- Ljava/util/Map<TX;+TY;>;
-- Ljava/lang/String;

parseSignature :: ReadP a -> Text -> a
parseSignature parse text = fst $ last $ readP_to_S parse $ T.unpack text

parseClassSignature :: IxConstPool -> Get Attr
parseClassSignature pool = do
  signature_index <- getWord16be
  let (CUTF8 signature) = getConstAt signature_index pool
  return $ ASignature $ ClassSig $ parseSignature parseClassSig signature

parseClassSig :: ReadP (ClassSignature TypeVariable)
parseClassSig = do
  tyVarDecls <- option [] parseTypeVariableDeclarations
  classParams <- some parseReferenceParameter
  return $ ClassSignature tyVarDecls classParams

parseMethodSignature :: IxConstPool -> Get Attr
parseMethodSignature pool = do
  signature_index <- getWord16be
  let (CUTF8 signature) = getConstAt signature_index pool
  return $ ASignature $ MethodSig $ parseSignature parseMethodSig signature

parseMethodSig :: ReadP (MethodSignature TypeVariable)
parseMethodSig = do
  tyVarDecls <- option [] parseTypeVariableDeclarations
  char '('
  methodParams <- many parseJavaType
  char ')'
  methodReturn <- fmap Just parseJavaType <++ (char 'V' >> return Nothing)
  throwsExceptions <- many (char '^' >> parseReferenceParameter)
  return $ MethodSignature tyVarDecls methodParams methodReturn throwsExceptions

parseFieldSignature :: IxConstPool -> Get Attr
parseFieldSignature pool = do
  signature_index <- getWord16be
  let (CUTF8 signature) = getConstAt signature_index pool
      parseFieldSig = fmap FieldSignature parseReferenceParameter
  return $ ASignature $ FieldSig $ parseSignature parseFieldSig signature

parseTypeVariableDeclarations :: ReadP (TypeVariableDeclarations TypeVariable)
parseTypeVariableDeclarations = do
  char '<'
  tyVarDecls <- some parseTypeVariableDeclaration
  char '>'
  return tyVarDecls

parseTypeVariableDeclaration :: ReadP (TypeVariableDeclaration TypeVariable)
parseTypeVariableDeclaration = do
  identifier <- munch1Text (/= ':')
  bounds     <- many parseTypeParameterBound
  return $ TypeVariableDeclaration identifier bounds

parseTypeParameterBound :: ReadP (Bound TypeVariable)
parseTypeParameterBound = do
  char ':'
  refParam <- parseReferenceParameter
  return $ ExtendsBound refParam

parseReferenceParameter :: ReadP (ReferenceParameter TypeVariable)
parseReferenceParameter = parseGenericRefType
                      <++ parseSingleTypeVariable
                      <++ parseArrayRefType

parseGenericRefType :: ReadP (ReferenceParameter TypeVariable)
parseGenericRefType = do
  char 'L'
  identifier <- munch1Text (\c -> c /= '<' && c /= ';')
  typeArgs <- option [] $ do
    char '<'
    typeArgs <- some parseTypeParameter
    char '>'
    return typeArgs
  -- TODO: Parse generic inner classes
  char ';'
  return $ GenericReferenceParameter (IClassName identifier) typeArgs []

parseSingleTypeVariable :: ReadP (ReferenceParameter TypeVariable)
parseSingleTypeVariable = do
  char 'T'
  typeVariable <- munch1Text (/= ';')
  char ';'
  return $ VariableReferenceParameter typeVariable

parseArrayRefType :: ReadP (ReferenceParameter TypeVariable)
parseArrayRefType = do
  char '['
  param <- parseJavaType
  return $ ArrayReferenceParameter param

parseTypeParameter :: ReadP (TypeParameter TypeVariable)
parseTypeParameter = fmap WildcardTypeParameter parseWildCard
                 <++ fmap SimpleTypeParameter parseReferenceParameter

parseWildCard :: ReadP (Bound TypeVariable)
parseWildCard = parseSimpleWildCard
            <++ parseGenExtendsClass
            <++ parseGenSuperClass

parseSimpleWildCard :: ReadP (Bound TypeVariable)
parseSimpleWildCard = do
  _ <- char '*'
  return NotBounded

parseGenExtendsClass :: ReadP (Bound TypeVariable)
parseGenExtendsClass = do
  char '+'
  refParam <- parseReferenceParameter
  return $ ExtendsBound $ refParam

parseGenSuperClass :: ReadP (Bound TypeVariable)
parseGenSuperClass = do
  char '-'
  refParam <- parseReferenceParameter
  return $ SuperBound $ refParam

parseJavaType :: ReadP (Parameter TypeVariable)
parseJavaType = fmap ReferenceParameter parseReferenceParameter
            <++ fmap PrimitiveParameter parsePrimitiveType

parsePrimitiveType :: ReadP PrimType
parsePrimitiveType = do
  x <- get
  case x of
    'B' -> return $ JByte
    'C' -> return $ JChar
    'D' -> return $ JDouble
    'F' -> return $ JFloat
    'I' -> return $ JInt
    'J' -> return $ JLong
    'S' -> return $ JShort
    'Z' -> return $ JBool
    _   -> fail "Nothing"

 -- <E:Ljava/lang/Object;>Ljava/lang/Object;Ljava/lang/Iterable<TE;>;
