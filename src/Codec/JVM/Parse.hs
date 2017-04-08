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


---------------------------------------------------------------------
----------------------Signature Parser------------------------------

data JavaTypeSignature = ReferenceTypeSignature | BaseType

data BaseType = B | C | D | F | I | J | S | Z

data ReferenceTypeSignature = ClassTypeSignature
                            | TypeVariableSignature
                            | ArrayTypeSignature

-- (java.util.Collection<+TE;>)V
parseRawSignature1 :: ReadP [Char]
parseRawSignature1 = do
  char 'L'
  char '('
  methodType <- (many $ satisfy (/= '<'))
  char '<'
  extendsOrSuper <- get
  char 'T'
  typeVariable <- get
  char ';'
  char '>'
  char ')'
  returnType <- look
  return returnType

--public String hello(String a, int a, int b)` -> `(Ljava/lang/String;II)Ljava/lang/String;

-- public String helloGeneric(T a, List<U> b, ArrayList<E> c) -> (TT;Ljava/util/List<TU;>;Ljava/util/ArrayList<TE;>;)Ljava/lang/String;

{-
public Map<X,? extends Y> helloGeneric(T a, List<? super X> b, ArrayList<? extends Y> c)

->

(TT;Ljava/util/List<-TX;>;Ljava/util/ArrayList<+TY;>;)Ljava/util/Map<TX;+TY;>;
-}


{-
public void loadClass(Class<?> clazz)` -> `(Ljava/lang/Class<*>;)V
-}

------------------------------------------------------------------------------
-----------------------------------------------------------
{-
1. Split method signature
2. parse parameter types
3. parse return types
-}
splitMethodSignature :: ReadP [Char]
splitMethodSignature = (between (char '(') (char ')') (many (satisfy (\c -> True))))

parseParameterType :: ReadP ()
parseParameterType = undefined

parseSimpleRefType :: ReadP [Char]
parseSimpleRefType = do
  x <- many (satisfy (/= ';'))
  return x

parseType :: ReadP [Char]
parseType = do
  
  return ""

parseGenericRefType :: ReadP [Char]
parseGenericRefType = do
  x <- (many $ satisfy (/= '<'))
  char '<'
  t <- many parseType
  char '>'
  return ""

--Ljava/util/Map<TX;+TY;>;
--Ljava/lang/String;
parseReferenceType :: ReadP [Char]
parseReferenceType = parseSimpleRefType <|> parseGenericRefType
  -- 
  -- remaining <- look
  -- if remaining == ""
  --   then return x -- contains all the stages :(
  --   else -> 

parseVoid :: ReadP [Char]
parseVoid = do
  return "VOID"

parsePrimitive :: ReadP [Char]
parsePrimitive = do
  return ""

parseReturnType :: ReadP [Char]
parseReturnType = do
  firstChar <- get
  case firstChar of
    'L' -> parseReferenceType
    'V' -> parseVoid
    'I' -> return "Integer"
    'B' -> return "Byte"



