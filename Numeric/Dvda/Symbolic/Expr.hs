{-# OPTIONS_GHC -Wall #-}

module Numeric.Dvda.Symbolic.Expr( Expr(..)
                                 , dim
                                 ) where

import Numeric.Dvda.Symbolic.Dim
import Numeric.Dvda.Symbolic.BinUn

data Expr a = ENum Dim [a]
            | EInt Dim [Int]
            | ESym Dim String
            | EUnary UnOp (Expr a)
            | EBinary BinOp (Expr a) (Expr a)
            | EBroadcast Dim (Expr a)
            | ERef Dim Int deriving Eq

instance Show a => Show (Expr a) where
  show (ENum D0 [x]) = show x
  show (ENum _ x) = show x
  show (EInt D0 [x]) = show x
  show (EInt _ x) = show x
  show (ESym d x) = replicate n '{' ++ x ++ (replicate n '}')
    where
      n = dorder d
  show (EUnary unaryType x) = showUnary x unaryType
  show (EBinary binaryType x y) = "(" ++ show x ++ " " ++ showBinary binaryType ++ " " ++ show y ++ ")"
  show (EBroadcast d x) = "BC(" ++ show d ++ " <- " ++ show x ++ ")"
  show (ERef _ k) = "REF(" ++ show k ++ ")"

-- Num instance
instance Num a => Num (Expr a) where
  fromInteger i = (EInt D0 [fromInteger i])
  abs x 
    | tIsI 0 x = broadcast (dim x) (EInt D0 [0])
    | otherwise = EUnary Abs x
  signum x 
    | tIsI 0 x = broadcast (dim x) (EInt D0 [0])
    | otherwise = EUnary Signum x
  negate x 
    | tIsI 0 x = broadcast (dim x) (EInt D0 [0])
    | otherwise = EUnary Neg x

  (+) = safeBinaryConstruct plus
    where
      plus (ENum dx xs) (ENum _ ys) = ENum dx $ zipWith (+) xs ys
      plus (EInt dx xs) (EInt _ ys) = EInt dx $ zipWith (+) xs ys
      plus x y
        | tIsI 0 x = y
        | tIsI 0 y = x
        | otherwise = EBinary Add x y

  (-) = safeBinaryConstruct minus
    where
      minus (ENum dx xs) (ENum _ ys) = ENum dx $ zipWith (-) xs ys
      minus (EInt dx xs) (EInt _ ys) = EInt dx $ zipWith (-) xs ys
      minus x y
        | tIsI 0 x = negate y
        | tIsI 0 y = x
        | otherwise = EBinary Sub x y

  (*) = safeBinaryConstruct times
    where
      times (ENum dx xs) (ENum _ ys) = ENum dx $ zipWith (*) xs ys
      times (EInt dx xs) (EInt _ ys) = EInt dx $ zipWith (*) xs ys
      times x y
        | tIsI 1 x = y
        | tIsI 1 y = x
        | tIsI 0 x || tIsI 0 y = broadcast (dim x) (EInt D0 [0])
        | otherwise = EBinary Mul x y


-- Fractional instance
instance Fractional a => Fractional (Expr a) where
  (/) = safeBinaryConstruct div'
    where
      div' (ENum d x) (ENum _ y) = ENum d $ zipWith (/) x y
      div' x y 
        | tIsI 0 y  = error "Tensor divide by zero"
        | tIsI 0 x  = broadcast (dim x) (EInt D0 [0])
        | otherwise = EBinary Div x y
  fromRational r = ENum D0 [fromRational r]


-- Floating instance
instance Floating a => Floating (Expr a) where
  pi = ENum D0 [pi]
  
  exp x  = EUnary Exp x
  sqrt x = EUnary Sqrt x
  log x  = EUnary Log x
  
  (**) = safeBinaryConstruct pow
    where
      pow x y 
        | tIsI 0 x && tIsI 0 y = error "indeterminate expression 0**0 encountered"
        | tIsI 0 y             = broadcast (dim x) (EInt D0 [1])
        | tIsI 1 y             = x
        | otherwise = EBinary Pow x y
  logBase = safeBinaryConstruct (EBinary LogBase)
  
  sin x = EUnary Sin x
  cos x = EUnary Cos x
  tan x = EUnary Tan x
                   
  asin x = EUnary ASin x
  acos x = EUnary ACos x
  atan x = EUnary ATan x

  sinh x = EUnary Sinh x
  cosh x = EUnary Cosh x
  tanh x = EUnary Tanh x

  asinh x = EUnary ASinh x
  acosh x = EUnary ACosh x
  atanh x = EUnary ATanh x

-- | all vector/vector and matrix/matrix dimension checking is done here, not in Num instances of Expr
safeBinaryConstruct :: Num a => (Expr a -> Expr a -> Expr a) -> Expr a -> Expr a -> Expr a
safeBinaryConstruct f x y
  -- normal combination
  | dim x == dim y = f x y
  -- broadcast scalar:
  | dim x == D0     = f (EBroadcast (dim y) x) y
  | dim y == D0     = f x (EBroadcast (dim x) y)
  -- dimension mismatch:
  | otherwise        = error $ unlines [ "Dimension mismatch in Expr + Expr"
                                       , "dim1: " ++ show (dim x)
                                       , "dim2: " ++ show (dim y)
                                       ]

-- | get dimensions of Expr
dim :: Expr a -> Dim
dim (ENum d _) = d
dim (EInt d _) = d
dim (ESym d _) = d
dim (ERef d _) = d
dim (EBroadcast d _) = d
dim (EUnary _ m) = dim m
dim (EBinary _ tx ty)
  | dim tx == dim ty = dim tx
  | otherwise        = error "api error - dim found mismatched dimensions in EBinary"

-- | test if tensor is broadcast from (EConst x)
tIsI :: Int -> Expr a -> Bool
tIsI i (EInt D0 [s]) = s == i
tIsI i (EBroadcast _ s) = tIsI i s
tIsI _ _ = False

broadcast :: Dim -> Expr a -> Expr a
broadcast D0 x@(ENum D0 [_]) = x -- take out these three lines
broadcast D0 x@(EInt D0 [_]) = x
broadcast D0 x@(ESym D0 _) = x
broadcast D0 _ = error "api error in broadcast"
broadcast d x = EBroadcast d x
