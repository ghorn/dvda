-- ArbitraryExpr.hs

{-# OPTIONS_GHC -Wall -fno-warn-orphans #-}

module Numeric.Dvda.Tests.ArbitraryExpr( Pair(..)
                                       , evalScalar
                                       ) where

import Test.QuickCheck

import Numeric.Dvda.Internal.Expr
import Numeric.Dvda.Internal.Tensor
import Numeric.Dvda.Symbolic(eval)
import Numeric.Dvda.Dim

data Pair a = Pair a (Expr a)

evalScalar :: Floating a => Expr a -> a
evalScalar expr
  | d == D0   = x
  | otherwise = error "evalScalar was not handed a scalar"
    where
      (d,x:[]) = eval expr 

--instance Arbitrary Char where
--    arbitrary     = choose ('\0', '\128')
    
instance Floating a => Show (Pair a) where
  show (Pair x ex) = "Pair\nFloating:   " ++ show x ++ "\nevaluated:  " ++ show (evalScalar ex) ++ "\nexpression: " ++ show ex

instance (Arbitrary a, Floating a) => Arbitrary (Pair a) where
  arbitrary = do
    
    sourceNum <- arbitrary
    sourceInt <- choose (-1000, 1000) -- dangerous, makes spurious failure for large ints improbably
    fx <- arbitrary
    fy <- arbitrary
    let (Pair x ex) = fx
        (Pair y ey) = fy
    
    frequency [ (12, return $! Pair sourceNum (Expr (TNum D0 [sourceNum])))
              , (12, return $! Pair (fromIntegral sourceInt) (Expr (TInt D0 [sourceInt])))
              , (5, return $! Pair (x * y) (ex * ey))
              , (5, return $! Pair (x + y) (ex + ey))
              , (5, return $! Pair (x - y) (ex - ey))
--              , (5, return $! Pair (x / y) (ex / ey))
              , (1, return $! Pair (abs x) (abs ex))
--              , (1, return $! Pair (signum x) (signum ex))
              , (1, return $! Pair (-x) (-ex))
--              , (1, return $! Pair (1/x) (1/ex))
              , (1, return $! Pair (exp x) (exp ex))
--              , (1, return $! Pair (sqrt x) (sqrt ex))
--              , (1, return $! Pair (log x) (log ex))
              , (1, return $! Pair (sin x) (sin ex))
              , (1, return $! Pair (cos x) (cos ex))
              , (1, return $! Pair (tan x) (tan ex))
--              , (1, return $! Pair (asin x) (asin ex))
--              , (1, return $! Pair (acos x) (acos ex))
--              , (1, return $! Pair (atan x) (atan ex))
              , (1, return $! Pair (sinh x) (sinh ex))
              , (1, return $! Pair (cosh x) (cosh ex))
              , (1, return $! Pair (tanh x) (tanh ex))
--              , (1, return $! Pair (asinh x) (asinh ex))
--              , (1, return $! Pair (acosh x) (acosh ex))
--              , (1, return $! Pair (atanh x) (atanh ex))
              ]

instance (Arbitrary a, Floating a) => Arbitrary (Expr a) where
  arbitrary = do
    fx <- arbitrary
    let (Pair _ ex) = fx
    return ex
  
--  shrink (Expr (TUnary _ x)) = [Expr x]
--  shrink (Expr (TBinary _ x y)) = [Expr x, Expr y] ++ shrink (Expr x) ++ shrink (Expr y)
--  shrink (Expr (TBroadcast _ x)) = [Expr x]
--  shrink _ = []
