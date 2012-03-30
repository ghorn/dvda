{-# OPTIONS_GHC -Wall #-}
{-# Language TypeFamilies #-}
{-# Language MultiParamTypeClasses #-}
{-# Language GADTs #-}
{-# Language FlexibleInstances #-}

module Ideas.StrongExpr( Expr(..)
                       , symE
                       , vsymE
                       , msymE
                       , dot
                       , deriv
                       , grad
                       , jacob
                       , hess
                       , dim
                       ) where

import Data.Array.Repa(DIM0,DIM1,DIM2,Z(..),(:.)(..), showShape, listOfShape,shapeOfList,Shape) -- hiding ((++))
import Data.Vector.Unboxed(Vector, toList, Unbox)
import Ideas.BinUn

dim :: Expr d a -> d
dim (ESym d _) = d
dim (EConst d _) = d
dim (ESingleton _) = error "don't get ESingleton's dim ya goon"
dim (EUnary _ x) = dim x
dim (EBinary _ x1 _) = dim x1
dim (EScale _ y) = dim y
dim (EDot x y) = dotDims (dim x) (dim y)
dim (ERef d _) = d
dim (EDeriv _ _) = Z
dim (EGrad _ args) = dim args
dim (EJacob x args) = shapeOfList $ listOfShape (dim x) ++ listOfShape (dim args)


data Expr d a where
  ESym :: d -> String -> Expr d a
  EConst :: d -> Vector a -> Expr d a
  ESingleton :: a -> Expr d a
  EUnary :: UnOp -> Expr d a -> Expr d a
  EBinary :: BinOp -> Expr d a -> Expr d a -> Expr d a
  EScale :: Expr DIM0 a -> Expr d a -> Expr d a
  EDot :: (Dot d1 d2, d ~ DotT d1 d2) => Expr d1 a -> Expr d2 a -> Expr d a
  ERef :: d -> Int -> Expr d a

  EDeriv :: (d ~ DIM0) => Expr DIM0 a -> Expr DIM0 a -> Expr d a
  EGrad  :: (d ~ DIM1) => Expr DIM0 a -> Expr DIM1 a -> Expr d a
  EJacob :: (d ~ DIM2) => Expr DIM1 a -> Expr DIM1 a -> Expr d a

instance Num a => Num (Expr d a) where
  (*) = EBinary Mul  
  (+) = EBinary Add
  (-) = EBinary Sub
  abs = EUnary Abs
  signum = EUnary Signum
  fromInteger = ESingleton . fromInteger
  
instance Fractional a => Fractional (Expr d a) where
  (/) = EBinary Div
  fromRational = ESingleton . fromRational

instance Floating a => Floating (Expr d a) where
  pi    = ESingleton pi
  (**)  = EBinary Pow
  exp   = EUnary Exp
  log   = EUnary Log
  sin   = EUnary Sin
  cos   = EUnary Cos
  asin  = EUnary ASin
  atan  = EUnary ATan
  acos  = EUnary ACos
  sinh  = EUnary Sinh
  cosh  = EUnary Cosh
  asinh = error "no instance for asinh"
  atanh = error "no instance for atanh"
  acosh = error "no instance for acosh"

class (Shape d1, Shape d2) => Dot d1 d2 where
  type DotT d1 d2
  dotDims :: d1 -> d2 -> DotT d1 d2
  
dot :: (Dot d1 d2, DotT d1 d2 ~ d) => Expr d1 a -> Expr d2 a -> Expr d a
dot x y = EDot x y

instance Dot DIM2 DIM2 where -- matrix-matrix
  type DotT DIM2 DIM2 = DIM2
  dotDims d1 d2 
    | c1 == r2  = shapeOfList [r1, c2]
    | otherwise = error "MM dimension mismatch"
    where
      [r1,c1] = listOfShape d1
      [r2,c2] = listOfShape d2
  
instance Dot DIM1 DIM1 where -- vector-vector
  type DotT DIM1 DIM1 = DIM0
  dotDims d1 d2 
    | r1 == r2  = Z
    | otherwise = error "VV dimension mismatch"
    where
      [r1] = listOfShape d1
      [r2] = listOfShape d2

instance Dot DIM2 DIM1 where -- matrix-vector
  type DotT DIM2 DIM1 = DIM1
  dotDims d1 d2 
    | c1 == r2  = shapeOfList [r1]
    | otherwise = error "MV dimension mismatch"
    where
      [r1,c1] = listOfShape d1
      [r2]    = listOfShape d2

instance Dot DIM1 DIM2 where -- vector-matrix
  type DotT DIM1 DIM2 = DIM1
  dotDims d1 d2 
    | c1 == r2  = shapeOfList [c2]
    | otherwise = error "VM dimension mismatch"
    where
      [c1]    = listOfShape d1
      [r2,c2] = listOfShape d2

paren :: Show a => a -> String
paren x = "( "++show x++" )"

instance (Shape d, Unbox a, Show a) => Show (Expr d a) where
  show (ESingleton x) = show x
  show (ESym d name) = name++"{"++showShape d++"}"
  show (EConst d x) = "{" ++ showShape d ++ ", "++show (toList x)++"}" 
  show (EUnary op x) = showUnary x op
  show (EBinary op x y) = paren x ++ showBinary op ++ paren y
  show (EScale s x) = paren s ++ "*" ++ paren x
  show (EDot _ _) = "EDot ?? ??"
  show (ERef d k) = "{ref:" ++ showShape d ++ ":" ++ show k ++ "}"
  show (EDeriv x y) = "deriv(" ++ show x ++ ", " ++ show y ++ ")"
  show (EGrad  x y) = "grad("  ++ show x ++ ", " ++ show y ++ ")"
  show (EJacob x y) = "jacob(" ++ show x ++ ", " ++ show y ++ ")"

symE :: String -> Expr DIM0 a
symE = ESym Z

vsymE :: Int -> String -> Expr DIM1 a
vsymE k = ESym (Z :. k)

msymE :: (Int,Int) -> String -> Expr DIM2 a
msymE (r,c) = ESym (Z :. r :. c)

deriv :: Expr DIM0 a -> Expr DIM0 a -> Expr DIM0 a
deriv = EDeriv

grad :: Expr DIM0 a -> Expr DIM1 a -> Expr DIM1 a
grad = EGrad

jacob :: Expr DIM1 a -> Expr DIM1 a -> Expr DIM2 a
jacob = EJacob

hess :: Expr DIM0 a -> Expr DIM1 a -> Expr DIM2 a
hess expr args = jacob (grad expr args) args
