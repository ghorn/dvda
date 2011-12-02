{- |
   Module      : Numeric.Dvda.Internal.Expr
   Description : Basic symbolic expression type

   This module instances 'Numeric.Dvda.Expr.Expr` as `Prelude.Num`\/`Prelude.Fractional`\/`Prelude.Floating`.
 -}

{-# OPTIONS_GHC -Wall #-}

module Numeric.Dvda.Internal.Expr( Expr(..)
                                 ) where

import Numeric.Dvda.Internal.Tensor

data Expr a = EScalar (Tensor a)
            | EVector (Tensor a)
            | EMatrix (Tensor a) deriving Eq


instance Show a => Show (Expr a) where
  show (EScalar x) = show x
  show (EVector x) = show x
  show (EMatrix x) = show x


instance Num a => Num (Expr a) where
  x + y = safeBinaryConstruct (+) x y
  x * y = safeBinaryConstruct (*) x y
  x - y = safeBinaryConstruct (-) x y
  abs = safeUnaryConstruct abs
  signum = safeUnaryConstruct signum
  fromInteger i = EScalar (TInt [] [fromInteger i])


instance Fractional a => Fractional (Expr a) where
  x / y = safeBinaryConstruct (/) x y
  fromRational r = EScalar $ TNum [] [fromRational r]


instance (Floating a) => Floating (Expr a) where
  pi = EScalar $ TNum [] [pi]
  
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
safeUnaryConstruct f (EScalar x) = EScalar $ f x
safeUnaryConstruct f (EVector x) = EVector $ f x
safeUnaryConstruct f (EMatrix x) = EMatrix $ f x

-- | all vector/vector and matrix/matrix dimension checking is done here, not in Num instanecs of Tensor
--   this is because those Num instances aren't exported and are only called through the Expr api
safeBinaryConstruct :: Num a => (Tensor a -> Tensor a -> Tensor a) -> Expr a -> Expr a -> Expr a
-- normal combination:
safeBinaryConstruct f (EScalar x) (EScalar y) = EScalar $ f x y
safeBinaryConstruct f (EVector x) (EVector y) 
  | tDim x == tDim y = EVector $ f x y
  | otherwise        = error $ unlines [ "Dimension mismatch in EVector + EVector"
                                       , "dim1: " ++ show (tDim x)
                                       , "v1: " ++ show x
                                       , ""
                                       , "dim2: " ++ show (tDim y)
                                       , "v2: " ++ show y
                                       ]
safeBinaryConstruct f (EMatrix x) (EMatrix y)
  | tDim x == tDim y = EMatrix $ f x y
  | otherwise        = error $ unlines [ "Dimension mismatch in EMatrix + EMatrix"
                                       , "dim1: " ++ show (tDim x)
                                       , "m1: " ++ show x
                                       , ""
                                       , "dim2: " ++ show (tDim y)
                                       , "m2: " ++ show y
                                       ]
-- broadcast scalar to vector:
safeBinaryConstruct f (EScalar x) (EVector y) = EVector $ f (TBroadcast (tDim y) x) y
safeBinaryConstruct f (EVector x) (EScalar y) = EVector $ f x (TBroadcast (tDim x) y)
-- broadcast scalar to matrix:
safeBinaryConstruct f (EScalar x) (EMatrix y) = EMatrix $ f (TBroadcast (tDim y) x) y
safeBinaryConstruct f (EMatrix x) (EScalar y) = EMatrix $ f x (TBroadcast (tDim x) y)
-- illegal combination:
safeBinaryConstruct _ (EVector _) (EMatrix _) = error "safeBinaryConstruct: Can't combine vector with matrix"
safeBinaryConstruct _ (EMatrix _) (EVector _) = error "safeBinaryConstruct: Can't combine vector with matrix"
