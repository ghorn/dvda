{- |
   Module      : Numeric.Dvda.Internal.Expr
   Description : Basic symbolic expression type

   This module instances 'Numeric.Dvda.Expr.Expr` as `Prelude.Num`\/`Prelude.Fractional`\/`Prelude.Floating`.
 -}

{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE Rank2Types #-}

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
  x + y = safeBinaryConstructNum (+) x y
  x * y = safeBinaryConstructNum (*) x y
  x - y = safeBinaryConstructNum (-) x y
  abs = safeUnaryConstructNum abs
  signum = safeUnaryConstructNum abs
  fromInteger i = EScalar (TInt [] [fromInteger i])


instance Fractional a => Fractional (Expr a) where
  x / y = safeBinaryConstructFrac (/) x y
  fromRational r = EScalar $ TNum [] [fromRational r]


instance (Floating a) => Floating (Expr a) where
  pi = EScalar $ TNum [] [pi]
  
  exp  = safeUnaryConstructFloating exp
  sqrt = safeUnaryConstructFloating sqrt
  log  = safeUnaryConstructFloating log
  
  (**) = safeBinaryConstructFloating (**)
  logBase = safeBinaryConstructFloating logBase
  
  sin = safeUnaryConstructFloating sin
  cos = safeUnaryConstructFloating cos
  tan = safeUnaryConstructFloating tan
                   
  asin = safeUnaryConstructFloating asin
  acos = safeUnaryConstructFloating acos
  atan = safeUnaryConstructFloating atan

  sinh = safeUnaryConstructFloating sinh
  cosh = safeUnaryConstructFloating cosh
  tanh = safeUnaryConstructFloating tanh

  asinh = safeUnaryConstructFloating asinh
  acosh = safeUnaryConstructFloating acosh
  atanh = safeUnaryConstructFloating atanh



--------------------------------------------------------------------
--------- please eliminate the following code duplication: ---------
--------------------------------------------------------------------
-- | all vector/vector and matrix/matrix dimension checking is done here, not in Num instanecs of Vector/Matrix
-- this is because those Num instances aren't exported and are only called through safeBinaryConstructNum
safeBinaryConstructNum :: Num a => (forall b . Num b => b -> b -> b) -> Expr a -> Expr a -> Expr a
-- normal combination:
safeBinaryConstructNum f (EScalar x) (EScalar y) = EScalar $ f x y
safeBinaryConstructNum f (EVector x) (EVector y) 
  | tDim x == tDim y = EVector $ f x y
  | otherwise        = error $ unlines [ "Dimension mismatch in EVector + EVector"
                                       , "v1: " ++ show x
                                       , ""
                                       , "v2: " ++ show y
                                       ]
safeBinaryConstructNum f (EMatrix x) (EMatrix y)
  | tDim x == tDim y = EMatrix $ f x y
  | otherwise        = error $ unlines [ "Dimension mismatch in EMatrix + EMatrix"
                                       , "m1: " ++ show x
                                       , ""
                                       , "m2: " ++ show y
                                       ]
-- broadcast scalar to vector:
safeBinaryConstructNum f (EScalar x) (EVector y) = EVector $ f (TBroadcast (tDim y) x) y
safeBinaryConstructNum f (EVector x) (EScalar y) = EVector $ f x (TBroadcast (tDim x) y)
-- broadcast scalar to matrix:
safeBinaryConstructNum f (EScalar x) (EMatrix y) = EMatrix $ f (TBroadcast (tDim y) x) y
safeBinaryConstructNum f (EMatrix x) (EScalar y) = EMatrix $ f x (TBroadcast (tDim x) y)
-- illegal combination:
safeBinaryConstructNum _ (EVector _) (EMatrix _) = error "safeBinaryConstructNum: Can't combine vector with matrix"
safeBinaryConstructNum _ (EMatrix _) (EVector _) = error "safeBinaryConstructNum: Can't combine vector with matrix"


safeUnaryConstructNum :: Num a => (forall b . Num b => b -> b) -> Expr a -> Expr a
safeUnaryConstructNum f (EScalar x) = EScalar $ f x
safeUnaryConstructNum f (EVector x) = EVector $ f x
safeUnaryConstructNum f (EMatrix x) = EMatrix $ f x


-- | all vector/vector and matrix/matrix dimension checking is done here, not in Num instanecs of Vector/Matrix
-- this is because those Num instances aren't exported and are only called through safeBinaryConstructFrac
safeBinaryConstructFrac :: Fractional a => (forall b . Fractional b => b -> b -> b) -> Expr a -> Expr a -> Expr a
-- normal combination:
safeBinaryConstructFrac f (EScalar x) (EScalar y) = EScalar $ f x y
safeBinaryConstructFrac f (EVector x) (EVector y) 
  | tDim x == tDim y = EVector $ f x y
  | otherwise        = error $ unlines [ "Dimension mismatch in EVector + EVector"
                                       , "v1: " ++ show x
                                       , ""
                                       , "v2: " ++ show y
                                       ]
safeBinaryConstructFrac f (EMatrix x) (EMatrix y)
  | tDim x == tDim y = EMatrix $ f x y
  | otherwise        = error $ unlines [ "Dimension mismatch in EMatrix + EMatrix"
                                       , "m1: " ++ show x
                                       , ""
                                       , "m2: " ++ show y
                                       ]
-- broadcast scalar to vector:
safeBinaryConstructFrac f (EScalar x) (EVector y) = EVector $ f (TBroadcast (tDim y) x) y
safeBinaryConstructFrac f (EVector x) (EScalar y) = EVector $ f x (TBroadcast (tDim x) y)
-- broadcast scalar to matrix:
safeBinaryConstructFrac f (EScalar x) (EMatrix y) = EMatrix $ f (TBroadcast (tDim y) x) y
safeBinaryConstructFrac f (EMatrix x) (EScalar y) = EMatrix $ f x (TBroadcast (tDim x) y)
-- illegal combination:
safeBinaryConstructFrac _ (EVector _) (EMatrix _) = error "safeBinaryConstructFrac: Can't combine vector with matrix"
safeBinaryConstructFrac _ (EMatrix _) (EVector _) = error "safeBinaryConstructFrac: Can't combine vector with matrix"


-- | all vector/vector and matrix/matrix dimension checking is done here, not in Num instanecs of Vector/Matrix
-- this is because those Num instances aren't exported and are only called through safeBinaryConstructFloating
safeBinaryConstructFloating :: Floating a => (forall b . Floating b => b -> b -> b) -> Expr a -> Expr a -> Expr a
-- normal combination:
safeBinaryConstructFloating f (EScalar x) (EScalar y) = EScalar $ f x y
safeBinaryConstructFloating f (EVector x) (EVector y) 
  | tDim x == tDim y = EVector $ f x y
  | otherwise        = error $ unlines [ "Dimension mismatch in EVector + EVector"
                                       , "v1: " ++ show x
                                       , ""
                                       , "v2: " ++ show y
                                       ]
safeBinaryConstructFloating f (EMatrix x) (EMatrix y)
  | tDim x == tDim y = EMatrix $ f x y
  | otherwise        = error $ unlines [ "Dimension mismatch in EMatrix + EMatrix"
                                       , "m1: " ++ show x
                                       , ""
                                       , "m2: " ++ show y
                                       ]
-- broadcast scalar to vector:
safeBinaryConstructFloating f (EScalar x) (EVector y) = EVector $ f (TBroadcast (tDim y) x) y
safeBinaryConstructFloating f (EVector x) (EScalar y) = EVector $ f x (TBroadcast (tDim x) y)
-- broadcast scalar to matrix:
safeBinaryConstructFloating f (EScalar x) (EMatrix y) = EMatrix $ f (TBroadcast (tDim y) x) y
safeBinaryConstructFloating f (EMatrix x) (EScalar y) = EMatrix $ f x (TBroadcast (tDim x) y)
-- illegal combination:
safeBinaryConstructFloating _ (EVector _) (EMatrix _) = error "safeBinaryConstructFloating: Can't combine vector with matrix"
safeBinaryConstructFloating _ (EMatrix _) (EVector _) = error "safeBinaryConstructFloating: Can't combine vector with matrix"


safeUnaryConstructFloating :: Floating a => (forall b . Floating b => b -> b) -> Expr a -> Expr a
safeUnaryConstructFloating f (EScalar x) = EScalar $ f x
safeUnaryConstructFloating f (EVector x) = EVector $ f x
safeUnaryConstructFloating f (EMatrix x) = EMatrix $ f x

--------------------------------------------------------------------------
--------------- (end of horrible hacky code duplication) -----------------
--------------------------------------------------------------------------
