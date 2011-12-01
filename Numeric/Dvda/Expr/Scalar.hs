-- Scalar.hs

{-# OPTIONS_GHC -Wall #-}

module Numeric.Dvda.Expr.Scalar( Scalar(..)
                               , sShowNode
                               , sGetSyms
                               , sIsI
                               ) where

import Numeric.Dvda.Expr.Binary
import Numeric.Dvda.Expr.Unary

data Scalar a = SNum a
              | SSym String
              | SUnary (Unary (Scalar a))
              | SBinary (Binary (Scalar a))
              | SInt Int deriving Eq


instance Show a => Show (Scalar a) where
  show (SNum x) = show x
  show (SInt x) = show x
  show (SSym x) = x
  show (SUnary x) = show x
  show (SBinary x) = show x

-- what will be displayed in the graphviz
sShowNode :: Show a => Scalar a -> String
--sShowNode (SNum x) = "{N}:" ++ show x
--sShowNode (SInt x) = "{I}:" ++ show x
sShowNode (SNum x) = show x
sShowNode (SInt x) = show x
sShowNode (SSym x) = x
sShowNode (SUnary (Unary unOp _)) = show unOp
sShowNode (SBinary (Binary binOp _ _)) = show binOp

sGetSyms :: Scalar a -> [Scalar a]
sGetSyms (SNum _) = []
sGetSyms (SInt _) = []
sGetSyms x@(SSym _) = [x]
sGetSyms (SUnary (Unary _ x)) = sGetSyms x
sGetSyms (SBinary (Binary _ x y)) = sGetSyms x ++ sGetSyms y


-- test if scalar == SInt x
sIsI :: Int -> Scalar a -> Bool
sIsI i (SInt si) = i == si
sIsI _ _ = False

-- num instance
instance Num a => Num (Scalar a) where
  fromInteger = error "API error: fromInteger (in Num a => Num (Vector a)) should not be accessible by the user"
  abs x 
    | sIsI 0 x = SInt 0
    | otherwise = SUnary (Unary Abs x)
  signum x 
    | sIsI 0 x = SInt 0
    | otherwise = SUnary (Unary Signum x)
  negate x 
    | sIsI 0 x = SInt 0
    | otherwise = SUnary (Unary Neg x)

  (SInt x) + (SInt y) = SInt $ x + y
  (SNum x) + (SNum y) = SNum $ x + y
  (SNum x) + (SInt y) = SNum $ x + (fromIntegral y)
  (SInt x) + (SNum y) = SNum $ (fromIntegral x) + y
  x + y
    | sIsI 0 x = y
    | sIsI 0 y = x
    | otherwise = SBinary (Binary Add x y)

  (SInt x) - (SInt y) = SInt $ x - y
  (SNum x) - (SNum y) = SNum $ x - y
  (SNum x) - (SInt y) = SNum $ x - (fromIntegral y)
  (SInt x) - (SNum y) = SNum $ (fromIntegral x) - y
  x - y
    | sIsI 0 x = negate y
    | sIsI 0 y = x
    | otherwise = SBinary (Binary Sub x y)

  (SInt x) * (SInt y) = SInt $ x * y
  (SNum x) * (SNum y) = SNum $ x * y
  (SNum x) * (SInt y) = SNum $ x * (fromIntegral y)
  (SInt x) * (SNum y) = SNum $ (fromIntegral x) * y
  x * y
    | sIsI 1 x = y
    | sIsI 1 y = x
    | sIsI 0 x || sIsI 0 y = SInt 0
    | otherwise = SBinary (Binary Add x y)

-- fractional instance
instance Fractional a => Fractional (Scalar a) where
  -- (SInt x) / (SInt y) = SNum $ fromRational (toInteger x % toInteger y)
  (SNum x) / (SNum y) = SNum $ x / y
  (SNum x) / (SInt y) = SNum $ x / (fromIntegral y)
  (SInt x) / (SNum y) = SNum $ (fromIntegral x) / y
  x / y = SBinary $ Binary Div x y
  fromRational = error "API error: fromRational (in Fractional a => Fractional (Vector a)) should not be accessible by the user"


-- floating instance
instance (Floating a) => Floating (Scalar a) where
  pi = error "API error: pi (in Floating a => Floating (Scalar a)) should not be accessible by the user"  
  
  exp x  = SUnary (Unary Exp x)
  sqrt x = SUnary (Unary Sqrt x)
  log x  = SUnary (Unary Log x)
  
  _ ** (SInt 0) = SInt 1
  x ** (SInt 1) = x
  x ** y = SBinary (Binary Pow x y)
  logBase x y = SBinary (Binary LogBase x y)
  
  sin x = SUnary (Unary Sin x)
  cos x = SUnary (Unary Cos x)
  tan x = SUnary (Unary Tan x)
                   
  asin x = SUnary (Unary ASin x)
  acos x = SUnary (Unary ACos x)
  atan x = SUnary (Unary ATan x)

  sinh x = SUnary (Unary Sinh x)
  cosh x = SUnary (Unary Cosh x)
  tanh x = SUnary (Unary Tanh x)

  asinh x = SUnary (Unary ASinh x)
  acosh x = SUnary (Unary ACosh x)
  atanh x = SUnary (Unary ATanh x)
