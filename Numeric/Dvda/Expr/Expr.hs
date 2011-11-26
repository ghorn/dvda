-- Expr.hs

{-# OPTIONS_GHC -Wall #-}

module Numeric.Dvda.Expr.Expr( Expr(..)
                             , symbolic
                             , Dim
                             ) where

import Data.Ratio(numerator, denominator)
import Data.GraphViz(Labellable(..))
import Data.Text.Lazy(pack)

import Numeric.Dvda.Expr.Op2Type
import Numeric.Dvda.Expr.ElemwiseType
import Numeric.Dvda.Expr.SourceType

type Dim = Int

data Expr a = Source { sourceType :: SourceType a
                     , dim :: Dim
                     }
            | Elemwise { elemwiseType :: ElemwiseType
                       , arg :: Expr a
                       , dim :: Dim
                       }
            | Op2 { op2Type :: Op2Type
                  , arg1 :: Expr a
                  , arg2 :: Expr a
                  , dim :: Dim
                  } deriving Eq

instance (Show a, Eq a) => Show (Expr a) where
  show op2@(Op2 {}) = "( " ++ show (arg1 op2) ++" "++ show (op2Type op2) ++" "++ show (arg2 op2) ++ " )"
  show src@(Source {}) = show $ sourceType src
  show ew@(Elemwise {}) = show (elemwiseType ew) ++ "(" ++ show (arg ew) ++ ")"

getCompatibleDims :: (Show a, Eq a) => Expr a -> Expr a -> Dim
getCompatibleDims x y
  | dim x == dim y = dim x
  | dim x == 0     = dim y
  | dim y == 0     = dim x
  | otherwise      = error $ "mismatching dimensions in\n"++show x++"\n"++show y

instance (Show a, Eq a, Num a) => Num (Expr a) where
  x + y = Op2 {op2Type = Add, arg1 = x, arg2 = y, dim = getCompatibleDims x y}
  x * y = Op2 {op2Type = Mul, arg1 = x, arg2 = y, dim = getCompatibleDims x y}
  x - y = Op2 {op2Type = Sub, arg1 = x, arg2 = y, dim = getCompatibleDims x y}
  negate x = Elemwise {elemwiseType = Neg, arg = x, dim = dim x}
  abs x = Elemwise {elemwiseType = Abs, arg = x, dim = dim x}
  signum x = Elemwise {elemwiseType = Signum, arg = x, dim = dim x}
  fromInteger x = Source {sourceType = I x, dim = 0}

instance Num a => Fractional (Expr a) where
  x / y = Op2 {op2Type = Div, arg1 = x, arg2 = y, dim = getCompatibleDims x y}
  fromRational x = num/den
    where
      num = fromIntegral $ numerator x
      den = fromIntegral $ denominator x

instance Floating a => Floating (Expr a) where
  pi = Source {sourceType = Number pi, dim = 0}
  
  exp x  = Elemwise { elemwiseType = Exp,  arg = x, dim = dim x }
  sqrt x = Elemwise { elemwiseType = Sqrt, arg = x, dim = dim x }
  log x  = Elemwise { elemwiseType = Log,  arg = x, dim = dim x }
  
  x**y = Op2 { op2Type = Pow, arg1 = x, arg2 = y, dim = getCompatibleDims x y }
  logBase x y = Op2 { op2Type = LogBase, arg1 = x, arg2 = y, dim = getCompatibleDims x y }
  
  sin x = Elemwise { elemwiseType = Sin, arg = x, dim = dim x }
  cos x = Elemwise { elemwiseType = Cos, arg = x, dim = dim x }
  tan x = Elemwise { elemwiseType = Tan, arg = x, dim = dim x }
                   
  asin x = Elemwise { elemwiseType = ASin, arg = x, dim = dim x }
  acos x = Elemwise { elemwiseType = ACos, arg = x, dim = dim x }
  atan x = Elemwise { elemwiseType = ATan, arg = x, dim = dim x }

  sinh x = Elemwise { elemwiseType = Sinh, arg = x, dim = dim x }
  cosh x = Elemwise { elemwiseType = Cosh, arg = x, dim = dim x }
  tanh x = Elemwise { elemwiseType = Tanh, arg = x, dim = dim x }

  asinh x = Elemwise { elemwiseType = ASinh, arg = x, dim = dim x }
  acosh x = Elemwise { elemwiseType = ACosh, arg = x, dim = dim x }
  atanh x = Elemwise { elemwiseType = ATanh, arg = x, dim = dim x }


instance (Show a, Num a) => Labellable (Expr a) where
  toLabelValue go = toLabelValue $ pack $ show go

symbolic :: String -> Expr a
symbolic name = Source {sourceType = Sym name, dim = 0}
