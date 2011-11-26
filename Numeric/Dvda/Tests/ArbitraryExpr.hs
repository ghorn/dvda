-- ArbitraryExpr.hs

{-# OPTIONS_GHC -Wall -fno-warn-orphans #-}

module Numeric.Dvda.Tests.ArbitraryExpr( Pair(..)
                                       ) where

import Test.QuickCheck

import Numeric.Dvda.Expr

data Pair a = Pair a (Expr a)

instance Floating a => Show (Pair a) where
  show (Pair x ex) = "Pair\nFloating:   " ++ show x ++ "\nevaluated:  " ++ show (evaluate ex) ++ "\nexpression: " ++ show ex

instance (Arbitrary a, Floating a) => Arbitrary (Pair a) where
  arbitrary = do
    sourceNum <- arbitrary
    sourceInteger <- arbitrary
    fx <- arbitrary
    fy <- arbitrary
    let (Pair x ex) = fx
        (Pair y ey) = fy
    
    frequency [ (12, return $! Pair sourceNum (Source {dim = 0, sourceType = Number sourceNum}))
              , (12, return $! Pair (fromIntegral sourceInteger) (Source {dim = 0, sourceType = I sourceInteger}))
              , (5, return $! Pair (x * y) (ex * ey))
              , (5, return $! Pair (x + y) (ex + ey))
              , (5, return $! Pair (x - y) (ex - ey))
              , (5, return $! Pair (x / y) (ex / ey))
              , (1, return $! Pair (abs x) (abs ex))
--              , (1, return $! Pair (signum x) (signum ex))
              , (1, return $! Pair (-x) (-ex))
              , (1, return $! Pair (1/x) (1/ex))
              , (1, return $! Pair (exp x) (exp ex))
              , (1, return $! Pair (sqrt x) (sqrt ex))
              , (1, return $! Pair (log x) (log ex))
              , (1, return $! Pair (sin x) (sin ex))
              , (1, return $! Pair (cos x) (cos ex))
              , (1, return $! Pair (tan x) (tan ex))
              , (1, return $! Pair (asin x) (asin ex))
              , (1, return $! Pair (acos x) (acos ex))
              , (1, return $! Pair (atan x) (atan ex))
              , (1, return $! Pair (sinh x) (sinh ex))
              , (1, return $! Pair (cosh x) (cosh ex))
              , (1, return $! Pair (tanh x) (tanh ex))
              , (1, return $! Pair (asinh x) (asinh ex))
              , (1, return $! Pair (acosh x) (acosh ex))
              , (1, return $! Pair (atanh x) (atanh ex))
              ]

instance (Arbitrary a, Floating a) => Arbitrary (Expr a) where
  arbitrary = do
    fx <- arbitrary
    let (Pair _ ex) = fx
    return ex
  
  shrink ew@(Elemwise {}) = [arg ew]
  shrink op2@(Op2 {}) = [arg1 op2, arg2 op2] ++ shrink (arg1 op2) ++ shrink(arg2 op2)
  shrink (Source {}) = []
