-- Expr.hs

{-# OPTIONS_GHC -Wall #-}

module Numeric.Dvda.Expr.Expr( Expr(..)
                             , sym
                             ) where

import Data.GraphViz(Labellable(..))
import Data.Text.Lazy(pack)

import Numeric.Dvda.Expr.Op2Type
import Numeric.Dvda.Expr.ElemwiseType
import Numeric.Dvda.Expr.SourceType

data Expr a = Source (SourceType a)
            | Elemwise ElemwiseType (Expr a)
            | Op2 Op2Type (Expr a) (Expr a) deriving Eq

instance (Show a, Eq a) => Show (Expr a) where
  show (Op2 op2Type x y) = "( " ++ show x ++" "++ show op2Type ++" "++ show y ++ " )"
  show (Source sourcetype) = show sourcetype
  show (Elemwise elemwiseType x) = show elemwiseType ++ show x

instance (Show a, Eq a, Num a) => Num (Expr a) where
  (+) = Op2 Add
  (*) = Op2 Mul
  (-) x y = Op2 Add x (-y)
  negate = Elemwise Neg
  abs = Elemwise Abs
  signum = Elemwise Signum
  fromInteger 0 = Source Zero
  fromInteger x = Source (I x)

instance (Show a, Num a) => Labellable (Expr a) where
  toLabelValue go = toLabelValue $ pack $ show go

sym :: String -> Expr a
sym name = Source (Sym name)
