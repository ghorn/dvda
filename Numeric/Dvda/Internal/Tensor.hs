-- Tensor.hs

{-# OPTIONS_GHC -Wall #-}

module Numeric.Dvda.Internal.Tensor( Tensor(..)
                                   , tShowNode
                                   , tGetSyms
                                   , tDim
                                   , tIsI
                                   , tToCCode
                                   , tEval
                                   ) where

import Numeric.Dvda.Internal.GNode
import Numeric.Dvda.Internal.Binary
import Numeric.Dvda.Internal.Unary
import Numeric.Dvda.Config(cType, cName)

data Tensor a = TNum [Int] [a]
              | TInt [Int] [Int]
              | TSym [Int] String
              | TUnary (Unary (Tensor a))
              | TBinary (Binary (Tensor a))
              | TBroadcast [Int] (Tensor a) deriving Eq

instance Show a => Show (Tensor a) where
  show (TNum [] [x]) = show x
  show (TNum _ x) = show x
  show (TInt [] [x]) = show x
  show (TInt _ x) = show x
  show (TSym d x) = replicate n '{' ++ x ++ (replicate n '}')
    where
      n = length d
  show (TUnary x) = show x
  show (TBinary x) = show x
  show (TBroadcast d x) = "BC(" ++ show d ++ " <- " ++ show x ++ ")"


-- what will be displayed in the graphviz
tShowNode :: Show a => Tensor a -> String
tShowNode x@(TNum _ _) = show x
tShowNode x@(TInt _ _) = show x
tShowNode x@(TSym _ _) = show x
tShowNode (TBroadcast d _) = "BC"++ show d
tShowNode (TUnary (Unary unOp _)) = show unOp
tShowNode (TBinary (Binary binOp _ _)) = show binOp

tGetSyms :: Tensor a -> [Tensor a]
tGetSyms (TNum _ _) = []
tGetSyms (TInt _ _) = []
tGetSyms x@(TSym _ _) = [x]
tGetSyms (TUnary (Unary _ x)) = tGetSyms x
tGetSyms (TBinary (Binary _ x y)) = tGetSyms x ++ (tGetSyms y)
tGetSyms (TBroadcast _ x) = tGetSyms x

-- | get dimensions of tensor
tDim :: Tensor a -> [Int]
tDim (TNum d _) = d
tDim (TInt d _) = d
tDim (TSym d _) = d
tDim (TBroadcast d _) = d
tDim (TUnary (Unary _ m)) = tDim m
tDim (TBinary (Binary _ tx ty)) 
  | tDim tx == tDim ty = tDim tx
  | otherwise          = error "api error - tDim found mismatched dimensions in TBinary"


-- | test if tensor is broadcast from (SInt x)
tIsI :: Int -> Tensor a -> Bool
tIsI i (TInt [] [s]) = s == i
tIsI i (TBroadcast _ s) = tIsI i s
tIsI _ _ = False

broadcast :: [Int] -> Tensor a -> Tensor a
broadcast [] x@(TNum [] [_]) = x
broadcast [] x@(TInt [] [_]) = x
broadcast [] x@(TSym [] _) = x
broadcast [] _ = error "api error in broadcast"
broadcast dim x = TBroadcast dim x

-- Num instance
instance Num a => Num (Tensor a) where
  fromInteger = error "API error: fromInteger (in Num a => Num (Tensor a)) should not be accessible by the user"
  abs x 
    | tIsI 0 x = broadcast (tDim x) (TInt [] [0])
    | otherwise = TUnary (Unary Abs x)
  signum x 
    | tIsI 0 x = broadcast (tDim x) (TInt [] [0])
    | otherwise = TUnary (Unary Signum x)
  negate x 
    | tIsI 0 x = broadcast (tDim x) (TInt [] [0])
    | otherwise = TUnary (Unary Neg x)
  (TNum dx xs) + (TNum _ ys) = TNum dx $ zipWith (+) xs ys
  (TInt dx xs) + (TInt _ ys) = TInt dx $ zipWith (+) xs ys
  x + y
    | tIsI 0 x = y
    | tIsI 0 y = x
    | otherwise = TBinary (Binary Add x y)
  (TNum dx xs) - (TNum _ ys) = TNum dx $ zipWith (-) xs ys
  (TInt dx xs) - (TInt _ ys) = TInt dx $ zipWith (-) xs ys
  x - y
    | tIsI 0 x = negate y
    | tIsI 0 y = x
    | otherwise = TBinary (Binary Sub x y)
  (TNum dx xs) * (TNum _ ys) = TNum dx $ zipWith (*) xs ys
  (TInt dx xs) * (TInt _ ys) = TInt dx $ zipWith (*) xs ys
  x * y
    | tIsI 1 x = y
    | tIsI 1 y = x
    | tIsI 0 x || tIsI 0 y = broadcast (tDim x) (TInt [] [0])
    | otherwise = TBinary (Binary Mul x y)

-- Fractional instance
instance Fractional a => Fractional (Tensor a) where
  (TNum d x) / (TNum _ y) = TNum d $ zipWith (/) x y
  x / y 
    | tIsI 0 y  = error "Tensor divide by zero"
    | tIsI 0 x  = broadcast (tDim x) (TInt [] [0])
    | otherwise = TBinary $ Binary Div x y
  fromRational = error "API error: fromRational (in Fractional a => Fractional (Tensor a)) should not be accessible by the user"

-- Floating instance
instance (Floating a) => Floating (Tensor a) where
  pi = error "API error: pi (in Floating a => Floating (Tensor a)) should not be accessible by the user"  
  
  exp x  = TUnary (Unary Exp x)
  sqrt x = TUnary (Unary Sqrt x)
  log x  = TUnary (Unary Log x)
  
  x ** y
    | tIsI 0 x && tIsI 0 y = error "indeterminate expression 0**0 encountered"
    | tIsI 0 y             = broadcast (tDim x) (TInt [] [1])
    | tIsI 1 y             = x
    | otherwise = TBinary (Binary Pow x y)
  logBase x y = TBinary (Binary LogBase x y)
  
  sin x = TUnary (Unary Sin x)
  cos x = TUnary (Unary Cos x)
  tan x = TUnary (Unary Tan x)
                   
  asin x = TUnary (Unary ASin x)
  acos x = TUnary (Unary ACos x)
  atan x = TUnary (Unary ATan x)

  sinh x = TUnary (Unary Sinh x)
  cosh x = TUnary (Unary Cosh x)
  tanh x = TUnary (Unary Tanh x)

  asinh x = TUnary (Unary ASinh x)
  acosh x = TUnary (Unary ACosh x)
  atanh x = TUnary (Unary ATanh x)


-- | evaluate (Tensor a) to a numeric list
tApply :: Floating a => Tensor a -> [a]
tApply (TNum _ x) = x
tApply (TInt _ x) = map fromIntegral x
tApply (TBroadcast d x) = replicate (product d) (head $ snd $ tEval x)
tApply (TSym _ _) = error "api error: mEval can't evaluate symbolic expression"
tApply (TUnary (Unary unType x)) = map (applyUnary unType) (tApply x)
tApply (TBinary (Binary binType x y)) = zipWith (applyBinary binType) (tApply x) (tApply y)

-- | evaluate (Tensor a) to a (dimension, value) tuple
tEval :: Floating a => Tensor a -> ([Int], [a])
tEval x@(TNum d _) = (d, tApply x)
tEval x@(TInt d _) = (d, tApply x)
tEval x@(TBroadcast d _) = (d, tApply x)
tEval (TSym _ _) = error "api error: tApply can't evaluate symbolic expression"
tEval x@(TUnary _) = (tDim x, tApply x)
tEval x@(TBinary _) = (tDim x, tApply x)


-- | convert GNode (Tensor a) into proper c code
tToCCode :: (Eq a, Show a) => GNode (Tensor a) -> String
tToCCode (GSource idx (TNum [] [x])) = assign idx ++ show x ++ ";"
tToCCode (GSource idx (TInt [] [x])) = assign idx ++ show x ++ ";"
tToCCode (GSource idx (TSym [] n)) = assign idx ++ n ++ ";"
tToCCode (GUnary idx (TUnary (Unary unType _)) ic) = assign idx ++ show unType ++ "(" ++ cName ic ++ ");"
tToCCode (GBinary idx (TBinary (Binary binType _ _)) (icx, icy)) = assign idx ++ 
                                                                   cName icx ++ 
                                                                   " " ++ show binType ++ " " ++
                                                                   cName icy ++";"
tToCCode _ = "#ERROR tensor c code gen not yet supported"
--tToCCode (GSource _ _) = "tToCCode api fail in GSource _ _)"
--tToCCode (GUnary _ _ _) = "tToCCode api fail in GUnary _ _)"
--tToCCode (GBinary _ _ _) = "tToCCode api fail in GBinary _ _)"

assign :: Int -> String
assign idx = cType ++ " " ++ cName idx ++ " = "


--tToCCode (GSource idx (SNum x)) = assign idx ++ show x ++ ";"
--tToCCode (GSource idx (SInt x)) = assign idx ++ show x ++ ";"
--tToCCode (GSource idx (SSym n)) = assign idx ++ n ++ ";"
--tToCCode (GUnary idx (SUnary (Unary unType _)) ic) = assign idx ++ show unType ++ "(" ++ cName ic ++ ");"
--tToCCode (GBinary idx (SBinary (Binary binType _ _)) (icx, icy)) = assign idx ++ 
--                                                               cName icx ++ 
--                                                               " " ++ show binType ++ " " ++
--                                                               cName icy ++";"
--tToCCode (GSource _ _) = "mToCCode api fail in GSource _ _)"
--tToCCode (GUnary _ _ _) = "mToCCode api fail in GUnary _ _)"
--tToCCode (GBinary _ _ _) = "mToCCode api fail in GBinary _ _)"
