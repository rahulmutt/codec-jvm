{-# LANGUAGE OverloadedStrings #-}
module Codec.JVM.Parse where

import Data.Binary.Get
import Data.Map.Strict (Map)
import Data.ByteString (ByteString)
import Data.ByteString.Lazy (toStrict, readFile)
import Data.Maybe (fromMaybe,fromJust)
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
import Codec.JVM.Types hiding (Super)
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
parseClassAttributes pool n = replicateM (fromIntegral n) $ parseClassAttribute pool

parseClassAttribute :: IxConstPool -> Get Attr
parseClassAttribute pool = do
  attribute_name_index <- getWord16be
  let CUTF8 attributeName = getConstAt attribute_name_index pool
  case attributeName of
    "Signature" -> parseClassSignature pool

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
    "Signature" -> parseFieldSignature pool

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
    "Signature" -> parseMethodSignature pool
    "MethodParameters" -> parseMethodParameters pool

parseMethodParameters :: IxConstPool -> Get Attr
parseMethodParameters pool = do
  getWord32be
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

-- TODO: Parse TypeVariable declarations and exceptions
parseMethodSignature :: IxConstPool -> Get Attr
parseMethodSignature pool = do
  getWord32be
  signature_index <- getWord16be
  let (CUTF8 signature) = getConstAt signature_index pool
      [(parameters,returnType)] = readP_to_S splitMethodSignature $ T.unpack signature
      parsedParameters  = readP_to_S parseParameterType parameters
      parsedReturnTypes = readP_to_S parseReturnType returnType
      (x,_) = parsedParameters !! ((length parsedParameters) - 1)
      (y,_) = parsedReturnTypes !! ((length parsedReturnTypes) - 1)
  return $ ASignature $ MethodSig $ MethodSignature [] x y []

parseFieldSignature :: IxConstPool -> Get Attr
parseFieldSignature pool = do
  getWord32be
  signature_index <- getWord16be
  let (CUTF8 signature) = getConstAt signature_index pool
      final = readP_to_S (parseReferenceType) $ T.unpack signature
      (x,_) = final !! ((length final) - 1)
      -- x: Just Parameter a
      ReferenceParameter y = fromJust x
  return $ ASignature $ FieldSig $ FieldSignature y

refs :: ReadP (ReferenceParameter TypeVariable)
refs = do
  char 'L'
  x <- parseReferenceType
  let ReferenceParameter y = fromJust x
  return y

parseClassSignature :: IxConstPool -> Get Attr
parseClassSignature pool = do
  getWord32be
  signature_index <- getWord16be
  let (CUTF8 signature) = getConstAt signature_index pool
      splitType = readP_to_S splitClassSignature $ T.unpack signature
  case length splitType of
    0 -> let parsedRes = readP_to_S (getAll refs) $ T.unpack signature
             (x,_) = parsedRes !! ((length parsedRes) - 1)
         in return $ ASignature $ ClassSig $ ClassSignature [] x
    _ -> let parsedParam = readP_to_S parseClassParams $ fst (splitType !! 0)
             parsedRes = readP_to_S (getAll refs) $ snd (splitType !! 0)
             (x,_) = parsedParam !! ((length parsedParam) - 1)
             (y,_) = parsedRes !! ((length parsedRes) - 1)
         in return $ ASignature $ ClassSig $ ClassSignature x y

parseClassParams :: ReadP (TypeVariableDeclarations TypeVariable)
parseClassParams = undefined --fmap Just $ getAll parseType
-- parseType :: ReadP (TypeParameter TypeVariable)

splitClassSignature :: ReadP [Char]
splitClassSignature = do
  char '<'
  x <- (many $ satisfy (/= '>'))
  char '>'
  return x

-- -----------------------------------------------------------
-- {-
-- 1. Split method signature
-- 2. parse parameter types
-- 3. parse return types
-- -}

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
parseParameterType :: ReadP [MethodParameter TypeVariable]
parseParameterType = getAll $ (fmap fromJust parseMethodReferenceType) <|> (fmap fromJust parsePrimitiveType)

parseMethodReferenceType :: ReadP (MethodReturn TypeVariable)
parseMethodReferenceType = (char 'L' >> parseGenericRefType)
                           <++ (char 'L' >> parseSimpleRefType)
                           <|> parseSingleTypeVariable

parsePrimitiveType :: ReadP (MethodReturn TypeVariable)
parsePrimitiveType = do
  x <- get
  case x of
    'B' -> return $ Just $ PrimitiveParameter JByte
    'C' -> return $ Just $ PrimitiveParameter JChar
    'D' -> return $ Just $ PrimitiveParameter JDouble
    'F' -> return $ Just $ PrimitiveParameter JFloat
    'I' -> return $ Just $ PrimitiveParameter JInt
    'J' -> return $ Just $ PrimitiveParameter JLong
    'S' -> return $ Just $ PrimitiveParameter JShort
    'Z' -> return $ Just $ PrimitiveParameter JBool


parseSimpleRefType :: ReadP (MethodReturn TypeVariable)
parseSimpleRefType = do
  x <- many (satisfy (/= ';'))
  return $ Just $ ReferenceParameter $ SimpleReferenceParameter $ IClassName $ showText x

-------------------------------------------------GENERICS----------------------------------------
--TODO: Parsing a single character for type variable. Need to parse string.
parseSimpleTypeVariable :: ReadP (TypeParameter TypeVariable)
parseSimpleTypeVariable = do
  char 'T'
  typeVariable <- get
  return $ SimpleTypeParameter (showText typeVariable) NotBounded

parseExtendsTypeVariable :: ReadP (TypeParameter TypeVariable)
parseExtendsTypeVariable = do
  char '+'
  char 'T'
  typeVariable <- get
  return $ WildcardTypeParameter $ Extends $ VariableReferenceParameter $ showText typeVariable

parseSuperTypeVariable :: ReadP (TypeParameter TypeVariable)
parseSuperTypeVariable = do
  char '-'
  char 'T'
  typeVariable <- get
  return $ WildcardTypeParameter $ Super $ VariableReferenceParameter $ showText typeVariable

parseWildCard :: ReadP (TypeParameter TypeVariable)
parseWildCard = do
  x <- char '*'
  return $ WildcardTypeParameter NotBounded

-- TODO: Only covered <E extends A>. Need to cober <E extends <A extend <..>>>
parseExtendsClass :: ReadP (TypeParameter TypeVariable)
parseExtendsClass = do
  typeVariable <- get
  char ':'
  char 'L'
  refType <- parseReferenceType
  -- refType :: Just (Parameter TypeVariable)
  let ReferenceParameter x = fromJust refType
  return $ SimpleTypeParameter (showText typeVariable) (Extends x)

parseSuperClass :: ReadP (TypeParameter TypeVariable)
parseSuperClass = undefined

parseType :: ReadP (TypeParameter TypeVariable)
parseType = parseSimpleTypeVariable
        <|> parseExtendsTypeVariable
        <|> parseSuperTypeVariable
        <|> parseWildCard
        <|> parseExtendsClass
        <|> parseSuperClass
-----------------------------------------------------------------------------------------------------------

parseGenericRefType :: ReadP (MethodReturn TypeVariable)
parseGenericRefType = do
  x <- (many $ satisfy (/= '<'))
  char '<'
  t <- getAll parseType
  char '>'
  return $ Just $ ReferenceParameter $ GenericReferenceParameter (IClassName $ showText x) t []

parseSingleTypeVariable :: ReadP (MethodReturn TypeVariable)
parseSingleTypeVariable = do
  char 'T'
  typeVariable <- get
  return $ Just $ ReferenceParameter $ VariableReferenceParameter $ showText typeVariable

--Ljava/util/Map<TX;+TY;>;
--Ljava/lang/String;
parseReferenceType :: ReadP (MethodReturn TypeVariable)
parseReferenceType = parseGenericRefType
                 <++ parseSimpleRefType
                 <|> parseSingleTypeVariable

parseReturnType :: ReadP (MethodReturn TypeVariable)
parseReturnType = do
  firstChar <- get
  case firstChar of
    'L' -> parseReferenceType
    'V' -> return Nothing
    'B' -> return $ Just $ PrimitiveParameter JByte
    'C' -> return $ Just $ PrimitiveParameter JChar
    'D' -> return $ Just $ PrimitiveParameter JDouble
    'F' -> return $ Just $ PrimitiveParameter JFloat
    'I' -> return $ Just $ PrimitiveParameter JInt
    'J' -> return $ Just $ PrimitiveParameter JLong
    'S' -> return $ Just $ PrimitiveParameter JShort
    'Z' -> return $ Just $ PrimitiveParameter JBool
