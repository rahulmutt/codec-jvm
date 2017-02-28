{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Codec.JVM.ASM.Code.Types where

import Codec.JVM.ASM.Code.CtrlFlow (CtrlFlow)

import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IntMap

newtype Label = Label Int
  deriving Show

mkLabel :: Int -> Label
mkLabel = Label

newtype Offset = Offset Int -- absolute
  deriving (Eq, Num, Show, Ord)

newtype StackMapTable = StackMapTable (IntMap CtrlFlow)
  deriving Show

-- Right-biased union
union' :: IntMap a -> IntMap a -> IntMap a
union' = IntMap.unionWith (flip const)

-- TODO: Implement a strict fold for mconcat
instance Monoid StackMapTable where
  mempty = StackMapTable mempty
  mappend (StackMapTable x) (StackMapTable y)
    = StackMapTable $ union' x y

insertSMT :: Int -> CtrlFlow -> StackMapTable -> StackMapTable
insertSMT k v (StackMapTable sm) = StackMapTable $ IntMap.insert k v sm

newtype LabelTable = LabelTable { unLabelTable :: IntMap Offset }
  deriving Show

instance Monoid LabelTable where
  mempty = LabelTable mempty
  mappend (LabelTable x) (LabelTable y)
    = LabelTable $ unionMax x y

unionMax :: (Ord a) => IntMap a -> IntMap a -> IntMap a
unionMax = IntMap.unionWith max

toLT :: [(Label, Offset)] -> LabelTable
toLT labels = LabelTable $ IntMap.fromList labels'
  where labels' = map (\(Label l, o) -> (l, o)) labels

unionsLT :: [LabelTable] -> LabelTable
unionsLT = LabelTable
         . foldlStrict unionMax mempty
         . map unLabelTable

insertLT :: Label -> Offset -> LabelTable -> LabelTable
insertLT (Label l) off (LabelTable lt) = LabelTable $ IntMap.insertWith max l off lt

lookupLT :: Label -> LabelTable -> Offset
lookupLT (Label l) (LabelTable lt) = IntMap.findWithDefault (Offset 0) l lt

-- Taken from containers package
foldlStrict :: (a -> b -> a) -> a -> [b] -> a
foldlStrict f = go
  where
    go z []     = z
    go z (x:xs) = let z' = f z x in z' `seq` go z' xs
{-# INLINE foldlStrict #-}
