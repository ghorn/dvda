{- |
   Module      : Numeric.Dvda.Internal.Expr
   Description : Basic symbolic expression type

   This module instances 'Numeric.Dvda.Expr.Expr` as `Prelude.Num`\/`Prelude.Fractional`\/`Prelude.Floating`.
 -}

{-# OPTIONS_GHC -Wall #-}

module Numeric.Dvda.Internal.Expr( Expr(..)
                                 ) where

import Numeric.Dvda.Internal.Tensor
import Numeric.Dvda.Dim

data Expr a = Expr (Tensor a) deriving Eq

instance Show a => Show (Expr a) where
  show (Expr x) = show x

instance Num a => Num (Expr a) where
  x + y = safeBinaryConstruct (+) x y
  x * y = safeBinaryConstruct (*) x y
  x - y = safeBinaryConstruct (-) x y
  abs = safeUnaryConstruct abs
  signum = safeUnaryConstruct signum
  fromInteger i = Expr (TInt D0 [fromInteger i])

instance Fractional a => Fractional (Expr a) where
  x / y = safeBinaryConstruct (/) x y
  fromRational r = Expr $ TNum D0 [fromRational r]

instance (Floating a) => Floating (Expr a) where
  pi = Expr $ TNum D0 [pi]
  
  exp  = safeUnaryConstruct exp
  sqrt = safeUnaryConstruct sqrt
  log  = safeUnaryConstruct log
  
  (**) = safeBinaryConstruct (**)
  logBase = safeBinaryConstruct logBase
  
  sin = safeUnaryConstruct sin
  cos = safeUnaryConstruct cos
  tan = safeUnaryConstruct tan
                   
  asin = safeUnaryConstruct asin
  acos = safeUnaryConstruct acos
  atan = safeUnaryConstruct atan

  sinh = safeUnaryConstruct sinh
  cosh = safeUnaryConstruct cosh
  tanh = safeUnaryConstruct tanh

  asinh = safeUnaryConstruct asinh
  acosh = safeUnaryConstruct acosh
  atanh = safeUnaryConstruct atanh

safeUnaryConstruct :: Num a => (Tensor a -> Tensor a) -> Expr a -> Expr a
safeUnaryConstruct f (Expr x) = Expr $ f x

-- | all vector/vector and matrix/matrix dimension checking is done here, not in Num instances of Tensor
--   this is because those Num instances aren't exported and are only called through the Expr api
safeBinaryConstruct :: Num a => (Tensor a -> Tensor a -> Tensor a) -> Expr a -> Expr a -> Expr a
safeBinaryConstruct f (Expr x) (Expr y)
  -- normal combination
  | tDim x == tDim y = Expr $ f x y
  -- broadcast scalar:
  | tDim x == D0     = Expr $ f (TBroadcast (tDim y) x) y
  | tDim y == D0     = Expr $ f x (TBroadcast (tDim x) y)
  -- dimension mismatch:
  | otherwise        = error $ unlines [ "Dimension mismatch in Expr + Expr"
                                       , "dim1: " ++ show (tDim x)
                                       , "dim2: " ++ show (tDim y)
                                       , "t1: " ++ show x
                                       , ""
                                       , "t2: " ++ show y
                                       ]
