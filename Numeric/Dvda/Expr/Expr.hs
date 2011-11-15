-- Expr.hs

{-# OPTIONS_GHC -Wall #-}

module Numeric.Dvda.Expr.Expr( Expr(..)
                             , sym
                             , showExprOp
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
  show (Elemwise elemwiseType x) = pre ++ show x ++ post
    where
      (pre, post) = elemwisePrePost elemwiseType

instance (Show a, Eq a, Num a) => Num (Expr a) where
  (+) = Op2 Add
  (-) = Op2 Sub
  (*) = Op2 Mul
  abs = Elemwise Abs
  signum = Elemwise Signum
  fromInteger 0 = Source Zero
  fromInteger x = Source (I x)

instance (Show a, Num a) => Labellable (Expr a) where
  toLabelValue go = toLabelValue $ pack $ show go

sym :: String -> Expr a
sym name = Source (Sym name)

showExprOp :: (Show a, Eq a) => Expr a -> String
showExprOp (Source s) = show s
showExprOp (Elemwise elemwiseType _) = (pre ++ "  " ++ post)
  where
    (pre, post) = elemwisePrePost elemwiseType
showExprOp (Op2 op2type _ _) = (show op2type)
