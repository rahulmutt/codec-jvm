module Codec.JVM.Field where

import Data.Binary.Put (Put)
import Data.Set (Set)

import qualified Data.List as L
import qualified Data.Set as S

import Codec.JVM.Attr (Attr, putAttr, unpackAttr)
import Codec.JVM.Const (Const(CUTF8))
import Codec.JVM.ConstPool (ConstPool, putIx)
import Codec.JVM.Internal (putI16)
import Codec.JVM.Types

-- https://docs.oracle.com/javase/specs/jvms/se8/html/jvms-4.html#jvms-4.5
data FieldInfo = FieldInfo
  { accessFlags :: Set AccessFlag
  , name :: UName
  , descriptor :: Desc
  , attributes :: [Attr] }
  deriving Show

unpackFieldInfo :: FieldInfo -> [Const]
unpackFieldInfo fi = unpackAttr =<< attributes fi

putFieldInfo :: String -> ConstPool -> FieldInfo -> Put
putFieldInfo debug cp fi = do
  putAccessFlags $ accessFlags fi
  case name fi of UName n       -> putIx (putFieldMsg "name") cp $ CUTF8 n
  case descriptor fi of Desc d  -> putIx (putFieldMsg "descriptor") cp $ CUTF8 d
  putI16 . L.length $ attributes fi
  mapM_ (putAttr (putFieldMsg "attributes") cp) $ attributes fi
  where putFieldMsg tag = "Field[" ++ tag ++ "][" ++ show (name fi)
                       ++ "][" ++ debug ++ "]"
