{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wall #-}

import Test.Framework (defaultMain, testGroup, Test(..))
import Test.Framework.Providers.QuickCheck2 (testProperty)

import Test.QuickCheck

import Data.Array.Repa(DIM0, DIM1, DIM2, Z(..), Shape, shapeOfList)
import Data.Hashable ( Hashable )

import Numeric.LinearAlgebra ( Matrix, Vector, Element, fromList, fromLists, Container, (><) )
import qualified Numeric.LinearAlgebra as LA
import Foreign.Storable (Storable)

import Control.Monad

import Dvda.BinUn
import Dvda.Expr

import Dvda.CallNative
import Dvda.Dual

import qualified Numeric.AD as AD

-- Arbitrary math operations

instance Arbitrary UnOp where
    arbitrary = oneof $ map return [minBound..maxBound]

instance Arbitrary BinOp where
    arbitrary = oneof $ map return [minBound..maxBound]

-- Arbitrary constants

class Shape sh => ArbSingleton sh where
    arbCSingleton :: Arbitrary a => Gen (Const sh a)

instance ArbSingleton Z where
    arbCSingleton = (liftM (CSingleton Z)) $ arbitrary

cvec xs = CVec (shapeOfList [length xs]) (fromList xs)

arbCVector :: (Storable a, Arbitrary a) => Gen (Const DIM1 a)
arbCVector = liftM cvec $ listOf1 $ arbitrary

cmat (r,c) xs 
  | r*c == sum (map length xs) && r == length xs = CMat (shapeOfList [c,r]) (fromLists xs)
  | otherwise = error $ "bad dims in mat!"++
                "\ngiven (r,c):  " ++ show (r,c) ++
                "\nactual (r,c): " ++ show (length xs, map length xs)

arbCMatrix :: (Element a, Arbitrary a) => Gen (Const DIM2 a)
arbCMatrix = do r <- choose (0, 100)
                c <- choose (0, 100)
                l <- vectorOf (r*c) arbitrary
                return $ cmat (r, c) l

instance Arbitrary a => Arbitrary (Const Z a) where
    arbitrary = arbCSingleton
    
instance (Arbitrary a, Storable a) => Arbitrary (Const DIM1 a) where
    arbitrary = arbCVector

instance (Arbitrary a, Element a) => Arbitrary (Const DIM2 a) where
    arbitrary = arbCMatrix

-- Arbitrary expressions

class Shape sh => ArbExpr sh where
    arbExpr :: Arbitrary a => Gen (Expr sh a)

arbConst :: (Shape sh, Arbitrary (Const sh a), Arbitrary a) => Gen (Expr sh a)
arbConst = liftM EConst arbitrary

arbUnary :: (Shape sh, Arbitrary (Expr sh a)) => Gen (Expr sh a)
arbUnary = liftM2 EUnary arbitrary arbitrary

arbBinary :: (Shape sh, Arbitrary (Expr sh a)) => Gen (Expr sh a)
arbBinary = liftM3 EBinary arbitrary arbitrary arbitrary

instance ArbExpr Z where
   arbExpr = oneof [arbConst, arbUnary, arbBinary]

instance Arbitrary a => Arbitrary (Expr Z a) where
   arbitrary = arbExpr

-- Arbitrary numerical functions

binary :: Floating a => [a -> a -> a]
binary = [(*), (+), (-), (/)]

unary :: Floating a => [a -> a]
unary = [abs, negate, signum, exp, sqrt, log, sin, cos, tan, asin, acos, atan, tanh, sinh, cosh, atanh, asinh, acosh]

-- We have to do this by hand because of all kinds of stupid type
-- shit.  Otherwise we'd map over unary.

cosProp, sinProp, tanProp, coshProp, sinhProp, tanhProp, expProp, signumProp, sqrtProp, negateProp, absProp :: (Eq a, Floating a, Num (Vector a), Show a, Hashable a, Container Vector a) => a -> Bool
cosProp x = cos x == nativeRun cos x
sinProp x = sin x == nativeRun sin x
tanProp x = tan x == nativeRun tan x
coshProp x = cosh x == nativeRun cosh x
sinhProp x = sinh x == nativeRun sinh x
tanhProp x = tanh x == nativeRun tanh x
expProp x = exp x == nativeRun exp x
signumProp x = signum x == nativeRun signum x
sqrtProp x = sqrt x == nativeRun sqrt x
negateProp x = negate x == nativeRun negate x
absProp x = abs x == nativeRun abs x

props :: [Double -> Bool]
props = [ cosProp, sinProp, tanProp
        , coshProp, sinhProp, tanhProp
        , expProp, signumProp, sqrtProp
        , negateProp, absProp
        ]

propNames :: [String]
propNames = [ "cos", "sin", "tan", "cosh", "sinh", "tanh", "exp", "signum", "sqrt", "negate", "abs"]

acosProp, asinProp, atanProp :: (Floating a, Num (Vector a), Ord a, Show a, Hashable a, Container Vector a) => a -> Property

acosProp x = x >= 0 && x <= 1 ==> acos x == nativeRun acos x
asinProp x = x >= 0 && x <= 1 ==> asin x == nativeRun asin x
atanProp x = x >= 0 && x <= 1 ==> atan x == nativeRun atan x

prop'Names :: [String]
prop'Names = ["acos", "asin", "atan"]

props' :: [Double -> Property]
props' = [ acosProp, asinProp, atanProp ]

mkUnaryTest (n, t) = testProperty ("unary_" ++ n) t

tests, uts :: [Test]
uts = (map mkUnaryTest (zip propNames props)) ++ (map mkUnaryTest (zip prop'Names props'))

tests = [
        testGroup "Unary functions 1" uts
        ]

main :: IO ()
main = defaultMain tests

