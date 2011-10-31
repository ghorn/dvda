-- Expr.hs

{-# OPTIONS_GHC -Wall #-}

module Had.Expr.Expr( Expr(..)
                    , sym
                    ) where

import Had.Expr.Op2Type
import Had.Expr.ElemwiseType
import Had.Expr.SourceType

data (Show a, Eq a) => Expr a = Source (SourceType a)
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

sym :: (Show a, Eq a) => String -> Expr a
sym name = Source (Sym name)
