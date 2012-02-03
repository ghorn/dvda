{- |
   Module      : Numeric.Dvda.Internal.Expr
   Description : Basic symbolic expression type

   This module instances 'Numeric.Dvda.Expr.Expr` as `Prelude.Num`\/`Prelude.Fractional`\/`Prelude.Floating`.
   .
   Expr exists as an extraction barrier for Tensor. Expr wraps Tensor and the user only ever sees/uses Expr. This lets us do all dimensionality checking on Expr and when working with Tensor we can assume that dimensions are correct.
 -}

{-# OPTIONS_GHC -Wall #-}

module Numeric.Dvda.Internal.Expr( Expr(..)
                                 , tmap
                                 ) where

import Numeric.Dvda.Internal.Tensor
import Numeric.Dvda.Dim

data Expr a = Expr (Tensor a) deriving Eq

-- like fmap but only for Expr (Tensor a)
tmap :: Num a => (Tensor a -> Tensor a) -> Expr a -> Expr a
tmap f (Expr x) = Expr $ f x

instance Show a => Show (Expr a) where
  show (Expr x) = show x

instance (Num a, Show a) => Num (Expr a) where
  x + y = safeBinaryConstruct (+) x y
  x * y = safeBinaryConstruct (*) x y
  x - y = safeBinaryConstruct (-) x y
  abs = tmap abs
  signum = tmap signum
  fromInteger i = Expr (TInt D0 [fromInteger i])

instance (Fractional a, Show a) => Fractional (Expr a) where
  x / y = safeBinaryConstruct (/) x y
  fromRational r = Expr $ TNum D0 [fromRational r]

instance (Floating a, Show a) => Floating (Expr a) where
  pi = Expr $ TNum D0 [pi]
  
  exp  = tmap exp
  sqrt = tmap sqrt
  log  = tmap log
  
  (**) = safeBinaryConstruct (**)
  logBase = safeBinaryConstruct logBase
  
  sin = tmap sin
  cos = tmap cos
  tan = tmap tan
                   
  asin = tmap asin
  acos = tmap acos
  atan = tmap atan

  sinh = tmap sinh
  cosh = tmap cosh
  tanh = tmap tanh

  asinh = tmap asinh
  acosh = tmap acosh
  atanh = tmap atanh

-- | all vector/vector and matrix/matrix dimension checking is done here, not in Num instances of Tensor
--   this is because those Num instances aren't exported and are only called through the Expr api
safeBinaryConstruct :: (Show a, Num a) => (Tensor a -> Tensor a -> Tensor a) -> Expr a -> Expr a -> Expr a
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
