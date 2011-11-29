-- Expr.hs

{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE Rank2Types #-}

module Numeric.Dvda.Expr.Expr( Expr(..)
                             , symbolic
                             , symVec
                             , symMat
                             , vec
                             , mat
                             ) where

--import Data.GraphViz(Labellable(..))
--import Data.Text.Lazy(pack)

import Numeric.Dvda.Expr.BinaryType
import Numeric.Dvda.Expr.UnaryType

data Binary a = Binary BinaryType a a deriving (Show, Eq)
data Unary a = Unary UnaryType a deriving (Show, Eq)

data Scalar a = SNum a
              | SSym String
              | SUnary (Unary (Scalar a))
              | SBinary (Binary (Scalar a))
              | SInt Int deriving (Eq, Show)

data Vector a = VNum Int [a]
              | VSym Int String
              | VUnary (Unary (Vector a))
              | VBinary (Binary (Vector a))
              | VBroadcast Int (Scalar a) deriving (Eq, Show)

data Matrix a = MNum (Int,Int) [a]
              | MSym (Int,Int) String
              | MUnary (Unary (Matrix a))
              | MBinary (Binary (Matrix a))
              | MBroadcast (Int,Int) (Scalar a) deriving (Eq, Show)

--class Exprlike a where
--  binaryConstructor :: Binary a -> a
--  unaryConstructor :: Unary a -> a
--  add :: a -> a -> a
  
vecDim :: Vector a -> Int
vecDim (VNum d _) = d
vecDim (VSym d _) = d
vecDim (VBroadcast d _) = d
vecDim (VUnary (Unary _ v)) = vecDim v
vecDim (VBinary (Binary _ vx vy)) 
  | vecDim vx == vecDim vy = vecDim vx
  | otherwise              = error "vecDim found mismatched dimensions in VBinary - this indicates the absence of proper checking during construction"


matDim :: Matrix a -> (Int, Int)
matDim (MNum d _) = d
matDim (MSym d _) = d
matDim (MBroadcast d _) = d
matDim (MUnary (Unary _ m)) = matDim m
matDim (MBinary (Binary _ mx my)) 
  | matDim mx == matDim my = matDim mx
  | otherwise              = error "matDim found mismatched dimensions in MBinary - this indicates the absence of proper checking during construction"



sIsZero :: Scalar a -> Bool
sIsZero (SInt 0) = True
sIsZero _ = False

vIsZero :: Vector a -> Bool
vIsZero (VBroadcast _ s) = sIsZero s
vIsZero _ = False

mIsZero :: Matrix a -> Bool
mIsZero (MBroadcast _ s) = sIsZero s
mIsZero _ = False


instance Num a => Num (Scalar a) where
  fromInteger = SInt . fromInteger
  abs x 
    | sIsZero x = SInt 0
    | otherwise = SUnary (Unary Abs x)
  signum x 
    | sIsZero x = SInt 0
    | otherwise = SUnary (Unary Signum x)
  negate x 
    | sIsZero x = SInt 0
    | otherwise = SUnary (Unary Neg x)

  (SInt x) + (SInt y) = SInt $ x + y
  (SNum x) + (SNum y) = SNum $ x + y
  (SNum x) + (SInt y) = SNum $ x + (fromIntegral y)
  (SInt x) + (SNum y) = SNum $ (fromIntegral x) + y
  x + y
    | sIsZero x = y
    | sIsZero y = x
    | otherwise = SBinary (Binary Add x y)
  (*) = undefined
  (-) = undefined


instance Num a => Num (Vector a) where
  fromInteger = error "API problem: fromInteger (in Num a => Num (Vector a)) should not be accessible by the user"
  abs x 
    | vIsZero x = VBroadcast (vecDim x) (SInt 0)
    | otherwise = VUnary (Unary Abs x)
  signum x 
    | vIsZero x = VBroadcast (vecDim x) (SInt 0)
    | otherwise = VUnary (Unary Signum x)
  negate x 
    | vIsZero x = VBroadcast (vecDim x) (SInt 0)
    | otherwise = VUnary (Unary Neg x)
  (VNum dx xs) + (VNum dy ys)
    | dx == dy  = VNum dx $ zipWith (+) xs ys
    | otherwise = error "Dimension mismatch in VNum + VNum"
  x + y
    | vIsZero x = y
    | vIsZero y = x
    | otherwise = VBinary (Binary Add x y)
  (*) = undefined
  (-) = undefined


instance Num a => Num (Matrix a) where
  fromInteger = error "API problem: fromInteger (in Num a => Num (Matrix a)) should not be accessible by the user"
  abs x 
    | mIsZero x = MBroadcast (matDim x) (SInt 0)
    | otherwise = MUnary (Unary Abs x)
  signum x 
    | mIsZero x = MBroadcast (matDim x) (SInt 0)
    | otherwise = MUnary (Unary Signum x)
  negate x 
    | mIsZero x = MBroadcast (matDim x) (SInt 0)
    | otherwise = MUnary (Unary Neg x)
  (MNum dx xs) + (MNum dy ys)
    | dx == dy  = MNum dx $ zipWith (+) xs ys
    | otherwise = error "Dimension mismatch in MNum + MNum"
  x + y
    | mIsZero x = y
    | mIsZero y = x
    | otherwise = MBinary (Binary Add x y)
  (*) = undefined
  (-) = undefined


safeBinaryConstruct :: Num a => (forall b . Num b => b -> b -> b) -> Expr a -> Expr a -> Expr a
-- normal combination:
safeBinaryConstruct f (EScalar x) (EScalar y) = EScalar $ f x y
safeBinaryConstruct f (EVector x) (EVector y) = EVector $ f x y
safeBinaryConstruct f (EMatrix x) (EMatrix y) = EMatrix $ f x y
-- broadcast scalar to vector:
safeBinaryConstruct f (EScalar x) (EVector y) = EVector $ f (VBroadcast (vecDim y) x) y
safeBinaryConstruct f (EVector x) (EScalar y) = EVector $ f x (VBroadcast (vecDim x) y)
-- broadcast scalar to matrix:
safeBinaryConstruct f (EScalar x) (EMatrix y) = EMatrix $ f (MBroadcast (matDim y) x) y
safeBinaryConstruct f (EMatrix x) (EScalar y) = EMatrix $ f x (MBroadcast (matDim x) y)
-- illegal combination:
safeBinaryConstruct _ (EVector _) (EMatrix _) = error "safeBinaryConstruct error: Can't combine vector with matrix"
safeBinaryConstruct _ (EMatrix _) (EVector _) = error "safeBinaryConstruct error: Can't combine vector with matrix"


safeUnaryConstruct :: Num a => (forall b . Num b => b -> b) -> Expr a -> Expr a
safeUnaryConstruct f (EScalar x) = EScalar $ f x
safeUnaryConstruct f (EVector x) = EVector $ f x
safeUnaryConstruct f (EMatrix x) = EMatrix $ f x

instance Num a => Num (Expr a) where
  x + y = safeBinaryConstruct (+) x y
  x * y = safeBinaryConstruct (*) x y
  x - y = safeBinaryConstruct (-) x y
  abs = safeUnaryConstruct abs
  signum = safeUnaryConstruct abs
  fromInteger i = EScalar (SInt (fromInteger i))
  

data Expr a = EScalar (Scalar a)
            | EVector (Vector a)
            | EMatrix (Matrix a) deriving (Eq, Show)


symbolic :: String -> Expr a
symbolic name = EScalar $ SSym name

symVec :: String -> Int -> Expr a
symVec name d = EVector $ VSym d name

symMat :: String -> (Int,Int) -> Expr a
symMat name d = EMatrix $ MSym d name

vec :: [a] -> Expr a
vec xs = EVector $ VNum (length xs) xs

mat :: (Int,Int) -> [a] -> Expr a
mat (r,c) xs 
  | length xs == r*c = EMatrix $ MNum (r,c) xs
  | otherwise        = error "Improper dimensions in mat :: (Int,Int) -> [a] -> Expr a"

--instance Show a => Show (Expr a) where
--  show (Sym _ name) = name
--  show (EScalar x) = show x
--  show (Vector _ v) = show v
--  show (Matrix _ m) = show m
--  show (Unary unaryType arg) = show unaryType ++ "(" ++ show arg ++ ")"
--  show (Binary binaryType arg1 arg2) = "( " ++ show arg1 ++" "++ show binaryType ++" "++ show arg2 ++ " )"
--  show (Broadcast d x) = "broadcast(" ++ show x ++ ", " ++ show d ++ ")"
----  show (Tensor t) = show t
----  show (Dot {arg1' = a1, arg2' = a2}) = "dot( " ++ show a1 ++ ", " ++ show a2 ++ " )"
--
--
--
--instance Fractional a => Fractional (Expr a) where
--  (EScalar x) / (EScalar y) = EScalar (x / y)
--  (EScalar x) / y = (broadcast x (getDim y)) / y
--  x / (EScalar y) = x / (broadcast y (getDim x))
--  x / y = zipBinary Div x y
--
--  fromRational x = EScalar $ fromRational x
--  fromRational x = num / den
--    where
--      num = fromIntegral $ numerator x
--      den = fromIntegral $ denominator x
--
--
--instance (Floating a) => Floating (Expr a) where
--  pi = EScalar $ N pi
--  
--  exp x  = mapUnary Exp x
--  sqrt x = mapUnary Sqrt x
--  log x  = mapUnary Log x
--  
--  x**y = zipBinary Pow x y
--  logBase x y = zipBinary Pow x y
--  
--  sin x = mapUnary Sin x
--  cos x = mapUnary Cos x
--  tan x = mapUnary Tan x
--                   
--  asin x = mapUnary ASin x
--  acos x = mapUnary ACos x
--  atan x = mapUnary ATan x
--
--  sinh x = mapUnary Sinh x
--  cosh x = mapUnary Cosh x
--  tanh x = mapUnary Tanh x
--
--  asinh x = mapUnary ASinh x
--  acosh x = mapUnary ACosh x
--  atanh x = mapUnary ATanh x
--
--
----instance (Show a, Num a, R.Elt a, Show sh, R.Shape sh) => Labellable (Expr sh a) where
----  toLabelValue go = toLabelValue $ pack $ show go ++ "["++show (getDim go)++"]"
--
