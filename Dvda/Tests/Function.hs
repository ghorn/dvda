{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}

module Dvda.Tests.Function ( Pair(..)
                           , runTests
                           ) where


import Test.QuickCheck

import Data.Hashable ( Hashable )
import Debug.Trace


import Dvda
import Dvda.Expr
import Dvda.CallNative

---- Arbitrary numerical functions
--binary :: Floating a => [a -> a -> a]
--binary = [(*), (+), (-), (/)]
--
--unary :: Floating a => [a -> a]
--unary = [abs, negate, signum, exp, sqrt, log, sin, cos, tan, asin, acos, atan, tanh, sinh, cosh, atanh, asinh, acosh]

data Pair a = Pair a (Expr Z a)

evalScalar :: (Eq a, Show a, Hashable a, Floating a) => Expr Z a -> a
evalScalar expr = getVal $ head $ nativeCall (\_ -> [expr]) (error "don't need any inputs in evalScalar")
  where
    getVal (EConst (CSingleton _ x)) = x
    getVal (EDimensionless x) = trace "test got EDimensionless" x
    getVal _ = error "evalScaler got non-scalar"

--instance Arbitrary Char where
--    arbitrary     = choose ('\0', '\128')
    
instance (Show a, Eq a, Hashable a, Floating a) => Show (Pair a) where
  show (Pair x ex) = "Pair\nFloating:   " ++ show x ++ "\nevaluated:  " ++ show (evalScalar ex) ++ "\nexpression: " ++ show ex

instance (Arbitrary a, RealFloat a) => Arbitrary (Pair a) where
  arbitrary = do
    sourceNum <- arbitrary
--    sourceInt <- choose (-1000, 1000) -- dangerous, makes spurious failure for large ints improbably
    fx <- arbitrary
    fy <- arbitrary
    let (Pair x ex) = fx
        (Pair y ey) = fy

    frequency $
      [ (24, return $! Pair sourceNum (EConst (CSingleton Z sourceNum)))
      , (5, return $! Pair (x * y) (ex * ey))
      , (5, return $! Pair (x + y) (ex + ey))
      , (5, return $! Pair (x - y) (ex - ey))
--      , (5, return $! Pair (x / y) (ex / ey))
      , (1, return $! Pair (abs x) (abs ex))
      , (1, return $! Pair (signum x) (signum ex))
      , (1, return $! Pair (-x) (-ex))
--      , (1, return $! Pair (1/x) (1/ex))
      , (1, return $! Pair (exp x) (exp ex))
      , (1, return $! Pair (sqrt (abs x)) (sqrt (abs ex)))
      , (1, return $! Pair (log (abs x + 1e-3)) (log (abs ex + 1e-3)))
      , (1, return $! Pair (sin x) (sin ex))
      , (1, return $! Pair (cos x) (cos ex))
      , (1, return $! Pair (tan x) (tan ex))
--      , (1, return $! Pair (asin x) (asin ex))
--      , (1, return $! Pair (acos x) (acos ex))
--      , (1, return $! Pair (atan x) (atan ex))
--      , (1, return $! Pair (sinh x) (sinh ex))
--      , (1, return $! Pair (cosh x) (cosh ex))
--      , (1, return $! Pair (tanh x) (tanh ex))
--      , (1, return $! Pair (asinh x) (asinh ex))
--      , (1, return $! Pair (acosh x) (acosh ex))
--      , (1, return $! Pair (atanh x) (atanh ex))
      ] -- `suchThat` (\(Pair x' _) -> not (isNaN x'))

--instance (Arbitrary a, Floating a) => Arbitrary (Expr Z a) where
--  arbitrary = do
--    fx <- arbitrary
--    let (Pair _ ex) = fx
--    return ex
  
--  shrink (Expr (TUnary _ x)) = [Expr x]
--  shrink (Expr (TBinary _ x y)) = [Expr x, Expr y] ++ shrink (Expr x) ++ shrink (Expr y)
--  shrink (Expr (TBroadcast _ x)) = [Expr x]
--  shrink _ = []


buildAndEvaluate :: (Show a, Hashable a, RealFloat a) => Pair a -> Bool
buildAndEvaluate (Pair x ex) = (x == evalScalar ex)

--evaluateEqualsCallNative :: RealFloat a => Expr Z a -> Bool
--evaluateEqualsCallNative expr = unsafePerformIO $ do
--  fun <- toFunction [] [expr]
--  return $ and $ zipWith (~=) [evalScalar expr] (map evalScalar (callNative fun []))
--
--callNativeEqualsCallC :: RealFloat a => Expr a -> Bool
--callNativeEqualsCallC expr = unsafePerformIO $ do
--  fun <- toFunction [] [expr]
--  return $ and $ zipWith (~=) (map evalScalar (callNative fun [])) (map evalScalar (callC fun []))
--
runTests :: IO ()
runTests = do
  putStrLn "============================================="
  putStrLn "======= running DVDA quickcheck tests ======="
  putStrLn "============================================="
--  sample (arbitrary :: Gen (Pair Double))
--  sample (arbitrary :: Gen (Expr Z Double))
  
  putStrLn "\nbuilding arbitrary expressions and evaluating them"
  quickCheck (buildAndEvaluate :: Pair Double -> Bool)
--  verboseCheck (buildAndEvaluate :: Pair Double -> Bool)
--
--  putStrLn "\nevaluate fun [] ~= callNative fun [] ..."
--  quickCheck (evaluateEqualsCallNative :: Expr Double -> Bool)
--
--  putStrLn "\ncallNative fun [] ~= callC fun [] ..."
--  quickCheck (callNativeEqualsCallC :: Expr Double -> Bool)
