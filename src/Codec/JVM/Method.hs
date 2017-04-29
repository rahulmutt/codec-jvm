module Codec.JVM.Method where

import Data.Set (Set)
import qualified Data.List as L

import Codec.JVM.Attr
import Codec.JVM.Const
import Codec.JVM.ASM.Code
import Codec.JVM.ConstPool
import Codec.JVM.Internal
import Codec.JVM.Types


-- https://docs.oracle.com/javase/specs/jvms/se8/html/jvms-4.html#jvms-4.6

data MethodInfo = MethodInfo
  { miAccessFlags :: Set AccessFlag
  , miName        :: UName
  , miDescriptor  :: Desc
  , miCode        :: Code
  , miAttributes  :: [Attr] }
  deriving Show

-- TODO: This is very ugly hack
unpackMethodInfo :: MethodInfo -> [Const]
unpackMethodInfo _  = [ CUTF8 $ attrName (ACode a a a a)
                      , CUTF8 $ attrName (AStackMapTable a)]
  where a = undefined

putMethodInfo :: ConstPool -> MethodInfo -> Put
putMethodInfo cp mi = do
  putAccessFlags $ miAccessFlags mi
  case miName mi of UName n       -> putIx cp $ CUTF8 n
  case miDescriptor mi of Desc d  -> putIx cp $ CUTF8 d
  putI16 . L.length $ attributes
  mapM_ (putAttr cp) attributes
  where attributes = toAttrs cp (miCode mi)
