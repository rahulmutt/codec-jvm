{-# LANGUAGE OverloadedStrings, BangPatterns, RecordWildCards #-}
module Codec.JVM.Attr where

import Data.Maybe (mapMaybe, maybe)
import Data.Monoid ((<>))
import Data.Map.Strict (Map)
import Data.ByteString (ByteString)
import Data.Foldable (traverse_)
import Data.Text (Text)
import Data.List (foldl', concat, nub)
import Data.Word(Word8, Word16)

import qualified Data.Map.Strict as Map
import qualified Data.Set as S
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import qualified Data.IntMap.Strict as IntMap
import qualified Data.Text as T

import Codec.JVM.ASM.Code.CtrlFlow
import qualified Codec.JVM.ASM.Code.CtrlFlow as CF
import Codec.JVM.ASM.Code (Code(..))
import Codec.JVM.ASM.Code.Instr (runInstrBCS)
import Codec.JVM.ASM.Code.Types (Offset(..), StackMapTable(..))
import Codec.JVM.Const (Const(..), constTag)
import Codec.JVM.ConstPool (ConstPool, putIx, unpack)
import Codec.JVM.Internal
import Codec.JVM.Types (PrimType(..), FieldType(..), IClassName(..),
                        AccessFlag(..), mkFieldDesc', putAccessFlags, prim)

type ParameterName = Text

type MParameter = (ParameterName, (S.Set AccessFlag))

type ClassName = Text

type InterfaceName = Text

type SuperClassName = Text

data Attr
  = ACode
    { maxStack  :: Int
    , maxLocals :: Int
    , code      :: ByteString
    , codeAttrs :: [Attr] }
  | AStackMapTable [(Offset, StackMapFrame)]
  | AInnerClasses InnerClassMap
  | AConstantValue Text
  | AMethodParam [MParameter]
  | ASignature (Signature TypeVariable)

------------------------Signatures------------------------------------
type TypeVariable = Text

data Signature a = ClassSig  (ClassSignature a)
                 | MethodSig (MethodSignature a)
                 | FieldSig  (FieldSignature a)
                 deriving Show

-- | JavaTypeSignature
data Parameter a
  = ReferenceParameter (ReferenceParameter a) -- ^ ReferenceTypeSignature
  | PrimitiveParameter PrimType               -- ^ BaseType
  deriving Show

type ObjectType = IClassName

-- | ReferenceTypeSignature
data ReferenceParameter a
  = -- | ClassTypeSignature
    GenericReferenceParameter
      ObjectType                     -- PackageSpecifier & SimpleClassTypeSignature
      [TypeParameter a]              -- TypeArguments
      [ReferenceParameter a]         -- ClassTypeSignatureSuffix
    -- | TypeVariableSignature
  | VariableReferenceParameter a
    -- | ArrayTypeSignature
  | ArrayReferenceParameter    (Parameter a)
  deriving Show

-- | TypeArgument, TypeParameter
data TypeParameter a
  = WildcardTypeParameter (Bound a) -- <?> <? extends A> <? super A>
  | SimpleTypeParameter (ReferenceParameter a)
  deriving Show

data Bound a
  = NotBounded
  | ExtendsBound (ReferenceParameter a)
  | SuperBound   (ReferenceParameter a)
  deriving Show

-- TypeParameters
type TypeVariableDeclarations a = [TypeVariableDeclaration a]

data TypeVariableDeclaration a = TypeVariableDeclaration a [Bound a]
  deriving Show

-- | ** ClassSignature **
data ClassSignature a
  = ClassSignature
      (TypeVariableDeclarations a)  -- TypeParameters
      [ClassParameter a]            -- SuperclassSignature & SuperinterfaceSignature
  deriving Show

type ClassParameter a = ReferenceParameter a

-- | ** MethodSignature **
data MethodSignature a =
  MethodSignature
    (TypeVariableDeclarations a)  -- TypeParameters
    [MethodParameter a]           -- JavaTypeSignature
    (MethodReturn a)              -- Result
    (ThrowsExceptions a)          -- ThrowsSignature
  deriving Show

-- | JavaTypeSignature
type MethodParameter a = Parameter a

-- | Result
type MethodReturn a = Maybe (Parameter a)

-- | ThrowsSignature
type ThrowsExceptions a = [ReferenceParameter a]

-- |  ** FieldSignature **
data FieldSignature a = FieldSignature (FieldParameter a)
  deriving Show

type FieldParameter a = ReferenceParameter a

mconcatMap :: (Monoid m) => (a -> m) -> [a] -> m
mconcatMap f xs = mconcat (map f xs)

generateSignature :: Signature TypeVariable -> Text
generateSignature sig = case sig of
  ClassSig (ClassSignature typeVarDecls classParams) ->
       generateTypeParameters typeVarDecls
    <> generateClassParameters classParams
  MethodSig (MethodSignature typeVarDecls methodParams methodReturn throwExceptions) ->
       generateTypeParameters typeVarDecls
    <> "(" <> mconcatMap generateParameter methodParams <> ")"
    <> maybe "V" generateParameter methodReturn
    <> generateThrowsSignature throwExceptions
  FieldSig (FieldSignature fieldRefParam) ->
       generateReferenceParameter fieldRefParam

generateThrowsSignature :: ThrowsExceptions TypeVariable -> Text
generateThrowsSignature = mconcat . map (T.cons '^' . generateReferenceParameter)

generateClassParameters :: [ReferenceParameter TypeVariable] -> Text
generateClassParameters = mconcat . map generateReferenceParameter

generateTypeParameters :: TypeVariableDeclarations TypeVariable -> Text
generateTypeParameters typeParams
  | length typeParams == 0 = ""
  | otherwise              = "<" <> mconcatMap generateTypeParameter typeParams <> ">"

generateTypeParameter :: TypeVariableDeclaration TypeVariable -> Text
generateTypeParameter (TypeVariableDeclaration identifier bounds)
  = identifier <> mconcatMap generateTypeParameterBound bounds

generateTypeParameterBound :: Bound TypeVariable -> Text
generateTypeParameterBound bounded = ":" <>
  case bounded of
    NotBounded -> ""
    ExtendsBound refParam -> generateReferenceParameter refParam
    SuperBound refParam -> generateReferenceParameter refParam

generateReferenceParameter :: ReferenceParameter TypeVariable -> Text
generateReferenceParameter (GenericReferenceParameter (IClassName className) typeArgs refParams) =
  "L" <> className <> generateTypeArguments typeArgs
      <> mconcatMap (T.cons '.' . generateSimpleClass) refParams <> ";"
  where generateSimpleClass (GenericReferenceParameter (IClassName simpleClassName) typeArgs []) =
          simpleClassName <> generateTypeArguments typeArgs
generateReferenceParameter (ArrayReferenceParameter param) =
  "[" <> generateParameter param
generateReferenceParameter (VariableReferenceParameter identifier) =
  "T" <> identifier <> ";"

generateParameter :: Parameter TypeVariable -> Text
generateParameter (ReferenceParameter refParam) =
  generateReferenceParameter refParam
generateParameter (PrimitiveParameter primType) =
  mkFieldDesc' (prim primType)

generateTypeArguments :: [TypeParameter TypeVariable] -> Text
generateTypeArguments typeParams
  | length typeParams == 0 = ""
  | otherwise              = "<" <> mconcatMap generateTypeArgument typeParams <> ">"

generateTypeArgument :: TypeParameter TypeVariable -> Text
generateTypeArgument (WildcardTypeParameter bound) =
  case bound of
    NotBounded       -> "*"
    ExtendsBound refParam -> "+" <> generateReferenceParameter refParam
    SuperBound refParam   -> "-" <> generateReferenceParameter refParam
generateTypeArgument (SimpleTypeParameter refParam) =
  generateReferenceParameter refParam

---------------------------------------------------------------------------

newtype InnerClassMap = InnerClassMap (Map Text InnerClass)
  deriving (Eq, Show)

innerClassElems :: InnerClassMap -> [InnerClass]
innerClassElems (InnerClassMap m) = Map.elems m

-- Left-biased monoid. Not commutative
instance Monoid InnerClassMap where
  mempty = InnerClassMap mempty
  mappend (InnerClassMap x) (InnerClassMap y) =
    InnerClassMap $ x `Map.union` y

instance Show Attr where
  show (AInnerClasses icm) = "AInnerClasses = " ++ show icm
  show attr = "A" ++ (T.unpack $ attrName attr)

attrName :: Attr -> Text
attrName (ACode _ _ _ _)    = "Code"
attrName (AStackMapTable _) = "StackMapTable"
attrName (AInnerClasses _)  = "InnerClasses"
attrName (ASignature _)     = "Signature"

unpackAttr :: Attr -> [Const]
unpackAttr attr = CUTF8 (attrName attr) : restAttributes
  where restAttributes =
          case attr of
            ACode _ _ _ xs -> concatMap unpackAttr xs
            ASignature sig -> [CUTF8 $ generateSignature sig]
            _              -> []

putAttr :: ConstPool -> Attr -> Put
putAttr cp attr = do
  putIx cp $ CUTF8 $ attrName attr
  let xs = runPut $ putAttrBody cp attr
  putI32 . fromIntegral $ LBS.length xs
  putByteString $ LBS.toStrict xs

putAttrBody :: ConstPool -> Attr -> Put
putAttrBody cp (ACode ms ls xs attrs) = do
  putI16 ms
  putI16 ls
  putI32 . fromIntegral $ BS.length xs
  putByteString xs
  putI16 0 -- TODO Exception table
  putI16 $ length attrs
  mapM_ (putAttr cp) attrs
putAttrBody cp (AStackMapTable xs) = do
  putI16 $ length xs
  putStackMapFrames cp xs
putAttrBody cp (AInnerClasses innerClassMap) = do
  putI16 $ length ics
  mapM_ (putInnerClass cp) ics
  where ics = innerClassElems innerClassMap
putAttrBody cp (ASignature signature) = do
  putIx cp $ CUTF8 $ generateSignature signature
putAttrBody cp attr = error $ "putAttrBody: Attribute not supported!\n"
                   ++ show attr

-- | http://docs.oracle.com/javase/specs/jvms/se8/html/jvms-4.html#jvms-4.7.4
--
-- Offsets are absolute (the delta conversion happen during serialization)

data StackMapFrame
  = SameFrame -- Covers normal & extended
  | SameLocals1StackItem !VerifType -- Covers normal & extended
  | ChopFrame !Int
  | AppendFrame !Word8 ![VerifType]
  | FullFrame ![VerifType] ![VerifType]
  deriving (Eq, Show)

putStackMapFrames :: ConstPool -> [(Offset, StackMapFrame)] -> Put
putStackMapFrames cp xs = (snd $ foldl' f (-1, return ()) xs)
  where f (offset, put) (Offset frameOffset, frame)
          = (frameOffset, put *> putFrame frame)
          where delta = frameOffset - (if offset == -1 then 0 else offset + 1)
                putVerifTy = putVerifType cp
                putFrame SameFrame =
                  if delta <= 63
                    then putWord8 $ fromIntegral delta
                    else do
                      putWord8 251
                      putWord16be $ fromIntegral delta
                putFrame (SameLocals1StackItem vt) = do
                  if delta <= 63
                    then putWord8 $ fromIntegral (delta + 64)
                    else do
                      putWord8 247
                      putWord16be $ fromIntegral delta
                  putVerifTy vt
                putFrame (ChopFrame k) = do
                  -- ASSERT (1 <= k <= 3)
                  putWord8 . fromIntegral $ 251 - k
                  putWord16be $ fromIntegral delta
                putFrame (AppendFrame k vts) = do
                  -- ASSERT (1 <= k <= 3)
                  putWord8 $ 251 + k
                  putI16 $ fromIntegral delta
                  traverse_ putVerifTy vts
                putFrame (FullFrame locals stack) = do
                  putWord8 255
                  putI16 $ fromIntegral delta
                  putI16 $ length locals
                  traverse_ putVerifTy locals
                  putI16 $ length stack
                  traverse_ putVerifTy stack

toAttrs :: ConstPool -> Code -> [Attr]
toAttrs cp code = [ACode maxStack' maxLocals' xs attrs]
  where (xs, cf, smt) = runInstrBCS (instr code) cp
        maxLocals' = CF.maxLocals cf
        maxStack' = CF.maxStack cf
        attrs = if null frames then [] else [AStackMapTable frames]
        frames = toStackMapFrames smt

toStackMapFrames :: StackMapTable -> [(Offset, StackMapFrame)]
toStackMapFrames (StackMapTable smt)
  = reverse . fst $ foldl' f ([], c) cfs
  where ((_,c):cfs) = IntMap.toAscList smt
        f (!xs, !cf') (!off, !cf) = ((Offset off, smf):xs, cf)
          where smf = generateStackMapFrame cf' cf

generateStackMapFrame :: CtrlFlow -> CtrlFlow -> StackMapFrame
generateStackMapFrame cf1 cf2
  | sameLocals && sz < 2
  = case sz of
      0 -> SameFrame
      1 -> SameLocals1StackItem stackTop
      _ -> fullFrame
  | otherwise
  = if sz == 0 then
      if lszdiff <= 3  && lszdiff > 0 then
        AppendFrame (fromIntegral lszdiff) $ drop lsz1 clocals2
      else if lszdiff >= -3 && lszdiff < 0 then
        ChopFrame (-lszdiff)
      else fullFrame
    else fullFrame
  where cf1' = mapLocals normaliseLocals cf1
        cf2' = mapLocals normaliseLocals cf2
        (clocals2, cstack2) = compressCtrlFlow cf2'
        (clocals1, _) = compressCtrlFlow cf1'
        fullFrame = FullFrame clocals2 cstack2
        stackTop = last cstack2
        sameLocals = areLocalsSame (locals cf1) (locals cf2)
        lsz1 = length clocals1
        lsz2 = length clocals2
        lszdiff = lsz2 - lsz1
        sz = length cstack2

data InnerClass =
  InnerClass { icInnerClass :: IClassName
             , icOuterClass :: IClassName
             , icInnerName  :: Text
             , icAccessFlags :: [AccessFlag] }
  deriving (Eq, Show)

putInnerClass :: ConstPool -> InnerClass -> Put
putInnerClass cp InnerClass {..} = do
  putIx cp $ CClass icInnerClass
  putIx cp $ CClass icOuterClass
  putIx cp $ CUTF8 icInnerName
  putAccessFlags $ S.fromList icAccessFlags

innerClassInfo :: [Const] -> ([Const], [Attr])
innerClassInfo consts = (nub. concat $ innerConsts, innerClassAttr)
  where
    innerClassAttr = if null innerClasses
                        then []
                        else [ AInnerClasses
                              . InnerClassMap
                              . Map.fromList
                              . map (\ic@InnerClass {..} ->
                                       (icInnerName, ic))
                              $ innerClasses]
    -- TODO: Support generation of private inner classes, not a big priority
    (innerConsts, innerClasses) = unzip $
      mapMaybe (\(CClass icn@(IClassName cn)) ->
                  case T.split (=='$') cn of
                    (outerClass:innerName:_)
                      | T.last innerName /= ';' ->
                        let innerClass =
                              InnerClass { icInnerClass = icn
                                         , icOuterClass = IClassName outerClass
                                         , icInnerName = innerName
                                         , icAccessFlags = [Public, Static] }
                        in Just (unpackInnerClass innerClass, innerClass)
                    _ -> Nothing)
        classConsts
    classConsts = filter (\c -> constTag c == 7) consts

unpackInnerClass :: InnerClass -> [Const]
unpackInnerClass InnerClass {..} =
  (CUTF8 icInnerName) :
    ((unpack $ CClass icOuterClass) ++ (unpack $ CClass icInnerClass))
