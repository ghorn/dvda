-- Vector.hs

{-# OPTIONS_GHC -Wall #-}

module Numeric.Dvda.Expr.Vector( Vector(..)
                               , vShowNode
                               , vGetSyms
                               , vecDim
                               , vIsI
                               , vToCCode
                               , vEval
                               ) where

import Numeric.Dvda.Expr.Binary
import Numeric.Dvda.Expr.Unary
import Numeric.Dvda.Expr.Scalar
import Numeric.Dvda.GNode

data Vector a = VNum Int [a]
              | VSym Int String
              | VUnary (Unary (Vector a))
              | VBinary (Binary (Vector a))
              | VBroadcast Int (Scalar a) deriving Eq

instance Show a => Show (Vector a) where
  show (VNum _ x) = show x
  show (VSym _ x) = "{" ++ x ++ "}"
  show (VUnary x) = show x
  show (VBinary x) = show x
  show (VBroadcast d x) = "BC( " ++ show x ++ " -> " ++ show d ++ " )"


-- what will be displayed in the graphviz
vShowNode :: Show a => Vector a -> String
vShowNode x@(VNum _ _) = show x
vShowNode x@(VSym _ _) = show x
vShowNode (VBroadcast d _) = "BC["++ show d ++ "]"
vShowNode (VUnary (Unary unOp _)) = show unOp
vShowNode (VBinary (Binary binOp _ _)) = show binOp


vGetSyms :: Vector a -> ([Vector a], [Scalar a])
vGetSyms (VNum _ _) = ([],[])
vGetSyms x@(VSym _ _) = ([x], [])
vGetSyms (VUnary (Unary _ x)) = vGetSyms x
vGetSyms (VBinary (Binary _ x y)) = (vx++vy, sx++sy)
  where
    (vx, sx) = vGetSyms x
    (vy, sy) = vGetSyms y
vGetSyms (VBroadcast _ x) = ([], sGetSyms x)


-- | get dimensions of vector
vecDim :: Vector a -> Int
vecDim (VNum d _) = d
vecDim (VSym d _) = d
vecDim (VBroadcast d _) = d
vecDim (VUnary (Unary _ v)) = vecDim v
vecDim (VBinary (Binary _ vx vy)) 
  | vecDim vx == vecDim vy = vecDim vx
  | otherwise              = error "vecDim found mismatched dimensions in VBinary - this indicates the absence of proper checking during construction"


-- test if vector is broadcast from (SInt x)
vIsI :: Int -> Vector a -> Bool
vIsI i (VBroadcast _ s) = sIsI i s
vIsI _ _ = False

-- Num instance
instance Num a => Num (Vector a) where
  fromInteger = error "API error: fromInteger (in Num a => Num (Vector a)) should not be accessible by the user"
  abs x 
    | vIsI 0 x = VBroadcast (vecDim x) (SInt 0)
    | otherwise = VUnary (Unary Abs x)
  signum x 
    | vIsI 0 x = VBroadcast (vecDim x) (SInt 0)
    | otherwise = VUnary (Unary Signum x)
  negate x 
    | vIsI 0 x = VBroadcast (vecDim x) (SInt 0)
    | otherwise = VUnary (Unary Neg x)

  (VNum dx xs) + (VNum _ ys) = VNum dx $ zipWith (+) xs ys
  x + y
    | vIsI 0 x = y
    | vIsI 0 y = x
    | otherwise = VBinary (Binary Add x y)
  x - y
    | vIsI 0 x = negate y
    | vIsI 0 y = x
    | otherwise = VBinary (Binary Sub x y)
  x * y
    | vIsI 1 x = y
    | vIsI 1 y = x
    | vIsI 0 x || vIsI 0 y = VBroadcast (vecDim x) (SInt 0)
    | otherwise = VBinary (Binary Mul x y)


-- Fractional instance
instance Fractional a => Fractional (Vector a) where
  (VNum d x) / (VNum _ y) = VNum d $ zipWith (/) x y
  x / y 
    | vIsI 0 y  = error "Vector divide by zero"
    | vIsI 0 x  = VBroadcast (vecDim x) (SInt 0)
    | otherwise = VBinary $ Binary Div x y

  fromRational = error "API error: fromRational (in Fractional a => Fractional (Vector a)) should not be accessible by the user"

-- Floating instance
instance (Floating a) => Floating (Vector a) where
  pi = error "API error: pi (in Floating a => Floating (Vector a)) should not be accessible by the user"  
  
  exp x  = VUnary (Unary Exp x)
  sqrt x = VUnary (Unary Sqrt x)
  log x  = VUnary (Unary Log x)
  
  _ ** (VBroadcast d (SInt 0)) = VBroadcast d (SInt 1)
  x ** (VBroadcast _ (SInt 1)) = x
  x ** y = VBinary (Binary Pow x y)
  logBase x y = VBinary (Binary LogBase x y)
  
  sin x = VUnary (Unary Sin x)
  cos x = VUnary (Unary Cos x)
  tan x = VUnary (Unary Tan x)
                   
  asin x = VUnary (Unary ASin x)
  acos x = VUnary (Unary ACos x)
  atan x = VUnary (Unary ATan x)

  sinh x = VUnary (Unary Sinh x)
  cosh x = VUnary (Unary Cosh x)
  tanh x = VUnary (Unary Tanh x)

  asinh x = VUnary (Unary ASinh x)
  acosh x = VUnary (Unary ACosh x)
  atanh x = VUnary (Unary ATanh x)

-- | evaluate (Vector a) to a numeric list
vApply :: Floating a => Vector a -> [a]
vApply (VNum _ x) = x
vApply (VBroadcast d x) = replicate d (sEval x)
vApply (VSym _ _) = error "api error: vEval can't evaluate symbolic expression"
vApply (VUnary (Unary unType x)) = map (applyUnary unType) (vApply x)
vApply (VBinary (Binary binType x y)) = zipWith (applyBinary binType) (vApply x) (vApply y)

-- | evaluate (Vector a) to a (dimension, value) tuple
vEval :: Floating a => Vector a -> (Int, [a])
vEval x@(VNum d _) = (d, vApply x)
vEval x@(VBroadcast d _) = (d, vApply x)
vEval (VSym _ _) = error "api error: vApply can't evaluate symbolic expression"
vEval x@(VUnary _) = (vecDim x, vApply x)
vEval x@(VBinary _) = (vecDim x, vApply x)


-- | convert GNode (Vector a) into proper c code
vToCCode :: (Eq a, Show a) => GNode (Vector a) -> String
vToCCode _ = "#ERROR vector c code gen not yet supported"
--vToCCode (GSource idx (SNum x)) = assign idx ++ show x ++ ";"
--vToCCode (GSource idx (SInt x)) = assign idx ++ show x ++ ";"
--vToCCode (GSource idx (SSym n)) = assign idx ++ n ++ ";"
--vToCCode (GUnary idx (SUnary (Unary unType _)) ic) = assign idx ++ show unType ++ "(" ++ cName ic ++ ");"
--vToCCode (GBinary idx (SBinary (Binary binType _ _)) (icx, icy)) = assign idx ++ 
--                                                               cName icx ++ 
--                                                               " " ++ show binType ++ " " ++
--                                                               cName icy ++";"
--vToCCode (GSource _ _) = "vToCCode api fail in GSource _ _)"
--vToCCode (GUnary _ _ _) = "vToCCode api fail in GUnary _ _)"
--vToCCode (GBinary _ _ _) = "vToCCode api fail in GBinary _ _)"
