-- Expr.hs

{-# OPTIONS_GHC -Wall #-}

module Numeric.Dvda.Expr.Expr( Expr(..)
                             , symbolic
                             , Dim
                             ) where

import Data.Ratio(numerator, denominator)
import Data.GraphViz(Labellable(..))
import Data.Text.Lazy(pack)

import Numeric.Dvda.Expr.BinaryType
import Numeric.Dvda.Expr.UnaryType
import Numeric.Dvda.Expr.SourceType

type Dim = Int

data Expr a = Source { sourceType :: SourceType a
                     , dim :: Dim
                     }
            | Unary { unaryType :: UnaryType
                    , arg :: Expr a
                    , dim :: Dim
                    }
            | Binary { binaryType :: BinaryType
                     , arg1 :: Expr a
                     , arg2 :: Expr a
                     , dim :: Dim
                     } deriving Eq

instance (Show a, Eq a) => Show (Expr a) where
  show binary@(Binary {}) = "( " ++ show (arg1 binary) ++" "++ show (binaryType binary) ++" "++ show (arg2 binary) ++ " )"
  show src@(Source {}) = show $ sourceType src
  show ew@(Unary {}) = show (unaryType ew) ++ "(" ++ show (arg ew) ++ ")"


-- takes expression and dimensions
-- broadcast expression to given dimensions
broadcast :: Expr a -> Dim -> Expr a
broadcast expr dim' = Unary { unaryType = Broadcast
                            , arg = expr
                            , dim = dim'
                            }

broadcastBinary :: Num a => (Expr a, Expr a) -> BinaryType -> Expr a
broadcastBinary (x, y) binaryt 
    | dim x == dim y = Binary {binaryType = binaryt, arg1 = x, arg2 = y, dim = dim x}
    | dim x == 0     = broadcast x (dim y) + y
    | dim y == 0     = x + broadcast y (dim x)
    | otherwise      = error $ "dimension mismatch in broadcastBinary (" ++ show binaryt ++ ")"

instance (Show a, Eq a, Num a) => Num (Expr a) where
  x + y = broadcastBinary (x,y) Add
  x * y = broadcastBinary (x,y) Mul
  x - y = broadcastBinary (x,y) Sub
  negate x = Unary {unaryType = Neg, arg = x, dim = dim x}
  abs x = Unary {unaryType = Abs, arg = x, dim = dim x}
  signum x = Unary {unaryType = Signum, arg = x, dim = dim x}
  fromInteger x = Source {sourceType = I x, dim = 0}

instance Num a => Fractional (Expr a) where
  x / y = broadcastBinary (x, y) Div
  fromRational x = num / den
    where
      num = fromIntegral $ numerator x
      den = fromIntegral $ denominator x

instance Floating a => Floating (Expr a) where
  pi = Source {sourceType = Number pi, dim = 0}
  
  exp x  = Unary { unaryType = Exp,  arg = x, dim = dim x }
  sqrt x = Unary { unaryType = Sqrt, arg = x, dim = dim x }
  log x  = Unary { unaryType = Log,  arg = x, dim = dim x }
  
  x**y = broadcastBinary (x,y) Pow
  logBase x y = broadcastBinary (x,y) LogBase
  
  sin x = Unary { unaryType = Sin, arg = x, dim = dim x }
  cos x = Unary { unaryType = Cos, arg = x, dim = dim x }
  tan x = Unary { unaryType = Tan, arg = x, dim = dim x }
                   
  asin x = Unary { unaryType = ASin, arg = x, dim = dim x }
  acos x = Unary { unaryType = ACos, arg = x, dim = dim x }
  atan x = Unary { unaryType = ATan, arg = x, dim = dim x }

  sinh x = Unary { unaryType = Sinh, arg = x, dim = dim x }
  cosh x = Unary { unaryType = Cosh, arg = x, dim = dim x }
  tanh x = Unary { unaryType = Tanh, arg = x, dim = dim x }

  asinh x = Unary { unaryType = ASinh, arg = x, dim = dim x }
  acosh x = Unary { unaryType = ACosh, arg = x, dim = dim x }
  atanh x = Unary { unaryType = ATanh, arg = x, dim = dim x }


instance (Show a, Num a) => Labellable (Expr a) where
  toLabelValue go = toLabelValue $ pack $ show go

symbolic :: String -> Expr a
symbolic name = Source {sourceType = Sym name, dim = 0}
