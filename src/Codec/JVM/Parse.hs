module Codec.JVM.Parse where

import Data.Binary.Get
import Data.Map.Strict (Map)
import Data.ByteString (ByteString)
import Data.ByteString.Lazy (toStrict, readFile)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Set (Set)
import Data.Word (Word32)

import qualified Data.List as L
import qualified Data.Text as T
import qualified Data.ByteString.Lazy as BL
import Control.Monad (when)

import Codec.JVM.Attr (Attr, putAttr)
import Codec.JVM.Const
import Codec.JVM.ConstPool
import Codec.JVM.Field (FieldInfo, putFieldInfo)
import Codec.JVM.Internal
import Codec.JVM.Method (MethodInfo, putMethodInfo)
import Codec.JVM.Types
import qualified Codec.JVM.ConstPool as CP

-- https://docs.oracle.com/javase/specs/jvms/se8/html/jvms-4.html#jvms-4.1
data ClassFile = ClassFile
  { constants   :: [Const]
  , version     :: Version
  , accessFlags :: Set AccessFlag
  , thisClass   :: IClassName
  , superClass  :: Maybe IClassName
  , interfaces  :: [IClassName]
  , fields      :: [FieldInfo]
  , methods     :: [MethodInfo]
  , attributes  :: Map Text Attr }
  deriving Show

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

