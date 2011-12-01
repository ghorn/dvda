-- Matrix.hs

{-# OPTIONS_GHC -Wall #-}

module Numeric.Dvda.Expr.Matrix( Matrix(..)
                               , mShowNode
                               , mGetSyms
                               , matDim
                               , mIsI
                               , mToCCode
                               ) where

import Numeric.Dvda.Expr.Binary
import Numeric.Dvda.Expr.Unary
import Numeric.Dvda.Expr.Scalar
import Numeric.Dvda.GNode

data Matrix a = MNum (Int,Int) [a]
              | MSym (Int,Int) String
              | MUnary (Unary (Matrix a))
              | MBinary (Binary (Matrix a))
              | MBroadcast (Int,Int) (Scalar a) deriving Eq

instance Show a => Show (Matrix a) where
  show (MNum _ x) = show x
  show (MSym _ x) = "{{" ++ x ++ "}}"
  show (MUnary x) = show x
  show (MBinary x) = show x
  show (MBroadcast d x) = "BC( " ++ show x ++ " -> " ++ show d ++ " )"

-- what will be displayed in the graphviz
mShowNode :: Show a => Matrix a -> String
mShowNode x@(MNum _ _) = show x
mShowNode x@(MSym _ _) = show x
mShowNode (MBroadcast d _) = "BC["++ show d ++ "]"
mShowNode (MUnary (Unary unOp _)) = show unOp
mShowNode (MBinary (Binary binOp _ _)) = show binOp

mGetSyms :: Matrix a -> ([Matrix a], [Scalar a])
mGetSyms (MNum _ _) = ([],[])
mGetSyms x@(MSym _ _) = ([x], [])
mGetSyms (MUnary (Unary _ x)) = mGetSyms x
mGetSyms (MBinary (Binary _ x y)) = (mx++my, sx++sy)
  where
    (mx, sx) = mGetSyms x
    (my, sy) = mGetSyms y
mGetSyms (MBroadcast _ x) = ([], sGetSyms x)

-- | get dimensions of matrix (rows, cols)
matDim :: Matrix a -> (Int, Int)
matDim (MNum d _) = d
matDim (MSym d _) = d
matDim (MBroadcast d _) = d
matDim (MUnary (Unary _ m)) = matDim m
matDim (MBinary (Binary _ mx my)) 
  | matDim mx == matDim my = matDim mx
  | otherwise              = error "matDim found mismatched dimensions in MBinary - this indicates the absence of proper checking during construction"


-- | test if matrix is broadcast from (SInt x)
mIsI :: Int -> Matrix a -> Bool
mIsI i (MBroadcast _ s) = sIsI i s
mIsI _ _ = False


-- Num instance
instance Num a => Num (Matrix a) where
  fromInteger = error "API error: fromInteger (in Num a => Num (Matrix a)) should not be accessible by the user"
  abs x 
    | mIsI 0 x = MBroadcast (matDim x) (SInt 0)
    | otherwise = MUnary (Unary Abs x)
  signum x 
    | mIsI 0 x = MBroadcast (matDim x) (SInt 0)
    | otherwise = MUnary (Unary Signum x)
  negate x 
    | mIsI 0 x = MBroadcast (matDim x) (SInt 0)
    | otherwise = MUnary (Unary Neg x)
  (MNum dx xs) + (MNum _ ys) = MNum dx $ zipWith (+) xs ys
  x + y
    | mIsI 0 x = y
    | mIsI 0 y = x
    | otherwise = MBinary (Binary Add x y)
  x - y
    | mIsI 0 x = negate y
    | mIsI 0 y = x
    | otherwise = MBinary (Binary Sub x y)
  x * y
    | mIsI 1 x = y
    | mIsI 1 y = x
    | mIsI 0 x || mIsI 0 y = MBroadcast (matDim x) (SInt 0)
    | otherwise = MBinary (Binary Mul x y)

-- Fractional instance
instance Fractional a => Fractional (Matrix a) where
  (MNum d x) / (MNum _ y) = MNum d $ zipWith (/) x y
  x / y = MBinary $ Binary Div x y
  fromRational = error "API error: fromRational (in Fractional a => Fractional (Matrix a)) should not be accessible by the user"

-- Floating instance
instance (Floating a) => Floating (Matrix a) where
  pi = error "API error: pi (in Floating a => Floating (Matrix a)) should not be accessible by the user"  
  
  exp x  = MUnary (Unary Exp x)
  sqrt x = MUnary (Unary Sqrt x)
  log x  = MUnary (Unary Log x)
  
  _ ** (MBroadcast d (SInt 0)) = MBroadcast d (SInt 1)
  x ** (MBroadcast _ (SInt 1)) = x
  x ** y = MBinary (Binary Pow x y)
  logBase x y = MBinary (Binary LogBase x y)
  
  sin x = MUnary (Unary Sin x)
  cos x = MUnary (Unary Cos x)
  tan x = MUnary (Unary Tan x)
                   
  asin x = MUnary (Unary ASin x)
  acos x = MUnary (Unary ACos x)
  atan x = MUnary (Unary ATan x)

  sinh x = MUnary (Unary Sinh x)
  cosh x = MUnary (Unary Cosh x)
  tanh x = MUnary (Unary Tanh x)

  asinh x = MUnary (Unary ASinh x)
  acosh x = MUnary (Unary ACosh x)
  atanh x = MUnary (Unary ATanh x)


-- | convert GNode (Matrix a) into proper c code
mToCCode :: (Eq a, Show a) => GNode (Matrix a) -> String
mToCCode _ = "#ERROR matrix c code gen not yet supported"
--mToCCode (GSource idx (SNum x)) = assign idx ++ show x ++ ";"
--mToCCode (GSource idx (SInt x)) = assign idx ++ show x ++ ";"
--mToCCode (GSource idx (SSym n)) = assign idx ++ n ++ ";"
--mToCCode (GUnary idx (SUnary (Unary unType _)) ic) = assign idx ++ show unType ++ "(" ++ cName ic ++ ");"
--mToCCode (GBinary idx (SBinary (Binary binType _ _)) (icx, icy)) = assign idx ++ 
--                                                               cName icx ++ 
--                                                               " " ++ show binType ++ " " ++
--                                                               cName icy ++";"
--mToCCode (GSource _ _) = "mToCCode api fail in GSource _ _)"
--mToCCode (GUnary _ _ _) = "mToCCode api fail in GUnary _ _)"
--mToCCode (GBinary _ _ _) = "mToCCode api fail in GBinary _ _)"
