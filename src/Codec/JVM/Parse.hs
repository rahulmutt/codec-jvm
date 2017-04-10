{-# LANGUAGE OverloadedStrings #-}
module Codec.JVM.Parse where

import Data.Binary.Get
import Data.Map.Strict (Map)
import Data.ByteString (ByteString)
import Data.ByteString.Lazy (toStrict, readFile)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Set (Set)
import Data.Map as Map
import Data.Char
import Data.Word (Word32,Word16,Word8)

import qualified Data.List as L
import qualified Data.Text as T
import qualified Data.ByteString.Lazy as BL
import Control.Monad (when,replicateM)
import Control.Applicative ((<|>))

import Codec.JVM.Attr
import Codec.JVM.Const
import Codec.JVM.ConstPool
import Codec.JVM.Field as F
import Codec.JVM.Internal
import Codec.JVM.Types
import qualified Codec.JVM.ConstPool as CP
import Text.ParserCombinators.ReadP

type ClassName = Text
type InterfaceName = Text

-- TODO: abstract out the replicateM bit

-- TODO: need to recycle/merge this with Method.hs
data MethodInfo = MethodInfo
  { mi_accessFlags :: Set AccessFlag
  , mi_name :: UName
  , mi_descriptor :: Desc
  , mi_attributes :: [Attr]}
  deriving Show

data Signature = Signature
  {  interfaces  :: [InterfaceName]
   , fieldInfos  :: [FieldInfo]
   , methodInfos :: [MethodInfo]
   , classAttributes :: [Attr]}
   deriving Show

mAGIC :: Word32
mAGIC = 0xCAFEBABE

parseClassFile :: Get (Map ClassName Signature)
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
  fieldsCount <- getWord16be
  fieldInfos <- parseFields pool fieldsCount -- :: [FieldInfo]
  methodsCount <- getWord16be
  methodInfos <- parseMethods pool methodsCount -- :: [MethodInfo]
  attributesCount <- getWord16be
  parseAttributes <- parseClassAttributes pool attributesCount
  return $
    insert iclsName
    Signature { interfaces  = interfaceNames
              ,fieldInfos  = fieldInfos
              ,methodInfos = methodInfos
              ,classAttributes = parseAttributes}
    Map.empty

parseClassAttributes :: IxConstPool -> Word16 -> Get [Attr]
parseClassAttributes pool n = replicateM (fromIntegral n) $ parseClassAttribute pool

parseClassAttribute :: IxConstPool -> Get Attr
parseClassAttribute pool = do
  attribute_name_index <- getWord16be
  let CUTF8 attributeName = getConstAt attribute_name_index pool
  case attributeName of
    "Signature" -> parseSignature pool

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
parseFieldAttributes pool n = replicateM (fromIntegral n) $ parseFieldAttribute pool

parseFieldAttribute :: IxConstPool -> Get Attr
parseFieldAttribute pool = do
  attribute_name_index <- getWord16be
  let CUTF8 attributeName = getConstAt attribute_name_index pool
  case attributeName of
    "ConstantValue" -> parseConstantValue pool
    "Signature" -> parseSignature pool

showText :: Show a => a -> Text
showText = T.pack . show

parseConstantValue :: IxConstPool -> Get Attr
parseConstantValue pool = do
  getWord32be
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
      mi_descriptor  = parseDescriptor cp descriptor_index
    }

parseMethodAttributes :: IxConstPool -> Word16 -> Get [Attr]
parseMethodAttributes pool n = replicateM (fromIntegral n) $ parseMethodAttribute pool

parseMethodAttribute :: IxConstPool -> Get Attr
parseMethodAttribute pool = do
  attribute_name_index <- getWord16be
  let (CUTF8 attribute_name) = getConstAt attribute_name_index pool
  case attribute_name of
    "Signature" -> parseSignature pool
    "MethodParameters" -> parseMethodParameters pool

parseMethodParameters :: IxConstPool -> Get Attr
parseMethodParameters pool = do
  getWord32be
  parameters_count <- getWord8
  parameters <- parseParameters pool parameters_count
  return $ AMethodParam parameters

parseParameters :: IxConstPool -> Word8 -> Get [Parameter]
parseParameters pool n = replicateM (fromIntegral n) $ parseParameter pool

parseParameter :: IxConstPool -> Get Parameter
parseParameter pool = do
  name_index <- getWord16be
  access_flags <- getAccessFlags ATMethodParam
  let CUTF8 parameterName = getConstAt name_index pool
  return (parameterName,access_flags)

parseSignature :: IxConstPool -> Get Attr
parseSignature pool = do
  getWord32be
  signature_index <- getWord16be
  let (CUTF8 signature) = getConstAt signature_index pool
  return $ ASignature signature



----------------------Signature Type------------------------------

-- data ASignature = CSignature | MSignature | FSignature
-- ASignature :: Attr

data MSignature = MSignature MParameterType MReturnType

type MParameterType = [MReturnType]
data MReturnType = JReferenceType JReferenceType | JPrimitiveType JPrimitiveType | SimpleTypeVariable SimpleTypeVariable |Void

data JReferenceType = SimpleClassName ClassName | GenericClassName ClassName [TypeParameter] | JRTSimpleTypeVariable JRTSimpleTypeVariable
data JPrimitiveType = B | C | D | F | I | J | S | Z

data TypeParameter = TPExtends TPSimpleTypeVariable | TPSuper TPSimpleTypeVariable | TPWildcard | TPSimpleTypeVariable TPSimpleTypeVariable

type SimpleTypeVariable    = Text
type JRTSimpleTypeVariable = Text
type TPSimpleTypeVariable  = Text
-----------------------------------------------------------
{-
1. Split method signature
2. parse parameter types
3. parse return types
-}

getAll :: ReadP a -> ReadP [a]
getAll p = many loop
  where
    loop = p <++ (get >> loop)

splitMethodSignature :: ReadP [Char]
splitMethodSignature = (between (char '(') (char ')') (many (satisfy (\c -> True))))

-- (Ljava/lang/String;II)
-- (TT;Ljava/util/List<TU;>;Ljava/util/ArrayList<TE;>;)
-- (TT;Ljava/util/List<-TX;>;Ljava/util/ArrayList<+TY;>;)
-- (Ljava/lang/Class<*>;)
parseParameterType :: ReadP MParameterType
parseParameterType = (char 'L' >> getAll parseReferenceType)
                 <|> getAll parsePrimitiveType

parsePrimitiveType :: ReadP MReturnType
parsePrimitiveType = do
  x <- get
  case x of
    'B' -> return $ JPrimitiveType B
    'C' -> return $ JPrimitiveType C
    'D' -> return $ JPrimitiveType D
    'F' -> return $ JPrimitiveType F
    'I' -> return $ JPrimitiveType I
    'J' -> return $ JPrimitiveType J
    'S' -> return $ JPrimitiveType S
    'Z' -> return $ JPrimitiveType Z


parseSimpleRefType :: ReadP MReturnType
parseSimpleRefType = do
  x <- many (satisfy (/= ';'))
  return $ JReferenceType $ SimpleClassName $ showText x

-------------------------------------------------GENERICS----------------------------------------
--TODO: Parsing a single character for type variable. Need to parse string.
parseSimpleTypeVariable :: ReadP TypeParameter
parseSimpleTypeVariable = do
  char 'T'
  typeVariable <- get
  return $ TPSimpleTypeVariable $ showText typeVariable

parseExtendsTypeVariable :: ReadP TypeParameter
parseExtendsTypeVariable = do
  char '+'
  char 'T'
  typeVariable <- get
  return $ TPExtends $ showText typeVariable

parseSuperTypeVariable :: ReadP TypeParameter
parseSuperTypeVariable = do
  char '-'
  char 'T'
  typeVariable <- get
  return $ TPSuper $ showText typeVariable

parseWildCard :: ReadP TypeParameter
parseWildCard = do
  x <- char '*'
  return TPWildcard

parseType :: ReadP TypeParameter
parseType = parseSimpleTypeVariable
        <|> parseExtendsTypeVariable
        <|> parseSuperTypeVariable
        <|> parseWildCard
---------------------------------------------------------------------------------------------------------

parseGenericRefType :: ReadP MReturnType
parseGenericRefType = do
  x <- (many $ satisfy (/= '<'))
  char '<'
  t <- getAll parseType
  char '>'
  return $ JReferenceType $ GenericClassName (showText x) t

parseSingleTypeVariable :: ReadP MReturnType
parseSingleTypeVariable = do
  char 'T'
  typeVariable <- get
  return $ SimpleTypeVariable $ showText typeVariable

--Ljava/util/Map<TX;+TY;>;
--Ljava/lang/String;
parseReferenceType :: ReadP MReturnType
parseReferenceType = parseGenericRefType
                 <++ parseSimpleRefType
                 <|> parseSingleTypeVariable

parseReturnType :: ReadP MReturnType
parseReturnType = do
  firstChar <- get
  case firstChar of
    'L' -> parseReferenceType
    'V' -> return Void
    'B' -> return $ JPrimitiveType B
    'C' -> return $ JPrimitiveType C
    'D' -> return $ JPrimitiveType D
    'F' -> return $ JPrimitiveType F
    'I' -> return $ JPrimitiveType I
    'J' -> return $ JPrimitiveType J
    'S' -> return $ JPrimitiveType S
    'Z' -> return $ JPrimitiveType Z
