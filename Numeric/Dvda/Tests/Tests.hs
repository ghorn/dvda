-- Tests.hs

{-# OPTIONS_GHC -Wall #-}

module Numeric.Dvda.Tests.Tests( runTests
                               ) where

import Test.QuickCheck
import System.IO.Unsafe

import Numeric.Dvda.Function
import Numeric.Dvda.Codegen.Codegen
import Numeric.Dvda.Simplify
import Numeric.Dvda.Expr
import Numeric.Dvda.Tests.ArbitraryExpr


(~=) :: RealFloat a => a -> a -> Bool
(~=) x y = (x == y) || isNaN x && isNaN y

buildAndEvaluate :: RealFloat a => Pair a -> Bool
buildAndEvaluate (Pair x ex) = (x == evaluate ex) || isNaN x && isNaN (evaluate ex)

evaluateEqSimplifyEvaluate :: RealFloat a => Pair a -> Bool
evaluateEqSimplifyEvaluate (Pair _ ex) = ex0 == exSimp || isNaN ex0
  where
    ex0 = evaluate ex
    exSimp = evaluate $ simplify ex
evaluateEqualsCallNative :: RealFloat a => Expr a -> Bool
evaluateEqualsCallNative expr = unsafePerformIO $ do
  fun <- toFunction [] [expr]
  return $ and $ zipWith (~=) [evaluate expr] (callNative fun [])

callNativeEqualsCallC :: RealFloat a => Expr a -> Bool
callNativeEqualsCallC expr = unsafePerformIO $ do
  fun <- toFunction [] [expr]
  return $ and $ zipWith (~=) (callNative fun []) (callC fun [])

runTests :: IO ()
runTests = do
  putStrLn "============================================="
  putStrLn "======= running DVDA quickcheck tests ======="
  putStrLn "============================================="
--  sample (arbitrary :: Gen (Pair Double))
--  sample (arbitrary :: Gen (Expr Double))
  
  putStrLn "\nbuilding arbitrary expressions and evaluating them"
  quickCheck (buildAndEvaluate :: Pair Double -> Bool)

  putStrLn "\nsimplify doesn't change result"
  quickCheck (evaluateEqSimplifyEvaluate :: Pair Double -> Bool)

  putStrLn "\nevaluate fun [] ~= callNative fun [] ..."
  quickCheck (evaluateEqualsCallNative :: Expr Double -> Bool)

  putStrLn "\ncallNative fun [] ~= callC fun [] ..."
  quickCheck (callNativeEqualsCallC :: Expr Double -> Bool)
