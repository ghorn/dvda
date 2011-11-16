-- Expr.hs

{-# OPTIONS_GHC -Wall #-}

module Numeric.Dvda.Expr.Expr( Expr(..)
                             , sym
                             ) where

import Data.Ratio(numerator, denominator)
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

instance Num a => Fractional (Expr a) where
  x / y = x*(Elemwise Inv y)
  fromRational x = num/den
    where
      num = fromIntegral $ numerator x
      den = fromIntegral $ denominator x

instance Floating a => Floating (Expr a) where
  pi = Source (Number pi)
  
  exp  = Elemwise Exp
  sqrt = Elemwise Sqrt
  log  = Elemwise Log
  
  (**) = Op2 Pow
  logBase = Op2 LogBase
  
  sin = Elemwise Sin
  cos = Elemwise Cos
  tan = Elemwise Tan

  asin = Elemwise ASin
  acos = Elemwise ACos
  atan = Elemwise ATan

  sinh = Elemwise Sinh
  cosh = Elemwise Cosh
  tanh = Elemwise Tanh

  asinh = Elemwise ASinh
  acosh = Elemwise ACosh
  atanh = Elemwise ATanh


instance (Show a, Num a) => Labellable (Expr a) where
  toLabelValue go = toLabelValue $ pack $ show go

sym :: String -> Expr a
sym name = Source (Sym name)
