{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}

module Dvda.Tests.Unary ( unaryTests
                        ) where

import Test.Framework ( Test, testGroup )
import Test.Framework.Providers.QuickCheck2 ( testProperty )

import Test.QuickCheck

import Data.Hashable ( Hashable )

import Numeric.LATC.NestedList ( Vector )

--import qualified Numeric.AD as AD

import Dvda.CallNative ( nativeRun )

-- Arbitrary numerical functions
binary :: Floating a => [a -> a -> a]
binary = [(*), (+), (-), (/)]

unary :: Floating a => [a -> a]
unary = [abs, negate, signum, exp, sqrt, log, sin, cos, tan, asin, acos, atan, tanh, sinh, cosh, atanh, asinh, acosh]

-- We have to do this by hand because of all kinds of stupid type
-- shit.  Otherwise we'd map over unary.

cosProp, sinProp, tanProp, coshProp, sinhProp, tanhProp, expProp, signumProp, negateProp, absProp :: (Eq a, Floating a, Show a, Hashable a) => a -> Bool
cosProp x = cos x == nativeRun cos x
sinProp x = sin x == nativeRun sin x
tanProp x = tan x == nativeRun tan x
coshProp x = cosh x == nativeRun cosh x
sinhProp x = sinh x == nativeRun sinh x
tanhProp x = tanh x == nativeRun tanh x
expProp x = exp x == nativeRun exp x
signumProp x = signum x == nativeRun signum x
negateProp x = negate x == nativeRun negate x
absProp x = abs x == nativeRun abs x

props :: [Double -> Bool]
props = [ cosProp, sinProp, tanProp
        , coshProp, sinhProp, tanhProp
        , expProp, signumProp
        , negateProp, absProp
        ]

propNames :: [String]
propNames = [ "cos", "sin", "tan", "cosh", "sinh", "tanh", "exp", "signum", "negate", "abs"]

acosProp, asinProp, atanProp, sqrtProp :: (Floating a, Num (Vector a), Ord a, Show a, Hashable a) => a -> Property

acosProp x = (x >= 0 && x <= 1) ==> abs (acos x - nativeRun acos x) < 1e-12
asinProp x = (x >= 0 && x <= 1) ==> asin x == nativeRun asin x
atanProp x = (x >= 0 && x <= 1) ==> atan x == nativeRun atan x
sqrtProp x = (x >= 0) ==> abs ((sqrt x) - (nativeRun sqrt x)) < 1e-13

prop'Names :: [String]
prop'Names = ["acos", "asin", "atan", "sqrt"]

props' :: [Double -> Property]
props' = [ acosProp, asinProp, atanProp, sqrtProp ]

mkUnaryTest :: Testable a => (String, a) -> Test
mkUnaryTest (n, t) = testProperty ("unary_" ++ n) t

uts :: [Test]
uts = (map mkUnaryTest (zip propNames props)) ++ (map mkUnaryTest (zip prop'Names props'))

unaryTests :: Test
unaryTests = testGroup "Unary functions 1" uts
