{-# OPTIONS_GHC -Wall #-}
{-# Language TypeFamilies #-}
{-# Language TypeOperators #-}
{-# Language GADTs #-}
{-# Language ConstraintKinds #-}
--{-# Language FlexibleContexts #-}
--{-# Language TypeSynonymInstances #-}
--{-# Language FlexibleInstances #-}
-- {-# MultiParamTypeClasses, FlexibleInstances #-}

module Casadi.Symbolic( Expr(..)
                      , sym
                      , vsym
                      , msym
                      , dot
                      ) where

import Data.Array.Repa hiding ((++))
import Dvda2.BinUn

instance Num (Expr a) where
  (*) = EBinary Mul  
  (+) = EBinary Add
  (-) = EBinary Sub
  abs = EUnary Abs
  signum = EUnary Signum
  fromInteger = EInt . fromInteger
  
instance Fractional (Expr a) where
  (/) = EBinary Div
  fromRational = EConst . fromRational

instance Floating (Expr a) where
  pi    = EConst pi
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

type family Dot a b
type instance Dot DIM2 DIM2 = DIM2 -- matrix-matrix
type instance Dot DIM1 DIM1 = DIM0 -- vector-vector
type instance Dot DIM0 DIM0 = DIM0 -- scalar-scalar

type instance Dot DIM2 DIM1 = DIM1 -- matrix-vector
type instance Dot DIM1 DIM2 = DIM1 -- vector-matrix

type instance Dot DIM0 DIM2 = DIM2 -- scalar-matrix
type instance Dot DIM2 DIM0 = DIM2 -- matrix-scalar

type instance Dot DIM0 DIM1 = DIM1 -- scalar-vector
type instance Dot DIM1 DIM0 = DIM1 -- vector-scalar

data Expr a where
  ESym :: a -> String -> Expr a
  EConst :: Double -> Expr a
  EInt :: Int -> Expr a
  EUnary :: UnOp -> Expr a -> Expr a
  EBinary :: BinOp -> Expr a -> Expr a -> Expr a
  EScale :: Expr DIM0 -> Expr a -> Expr a
  EDot :: (Dot b c ~ a) => Expr b -> Expr c -> Expr a
  EDeriv :: (a ~ DIM0) => Expr DIM0 -> Expr DIM0 -> Expr a
  EGrad  :: (a ~ DIM1) => Expr DIM0 -> Expr DIM1 -> Expr a
  EHess  :: (a ~ DIM2) => Expr DIM0 -> Expr DIM1 -> Expr a
  EJacob :: (a ~ DIM2) => Expr DIM1 -> Expr DIM1 -> Expr a

paren :: Show a => a -> String
paren x = "( "++show x++" )"

instance (Show a) => Show (Expr a) where
  show (ESym dim name) = name++"{"++show dim++"}"
  show (EConst x) = show x
  show (EInt k) = show k
  show (EUnary op x) = showUnary x op
  show (EBinary op x y) = paren x ++ showBinary op ++ paren y
  show (EScale s x) = paren s ++ "*" ++ paren x
  show (EDot _ _) = "EDot ?? ??"
  show (EDeriv x y) = "EDeriv(" ++ show x ++ ", " ++ show y ++ ")"
  show (EGrad  x y) = "EGrad("  ++ show x ++ ", " ++ show y ++ ")"
  show (EHess  x y) = "EHess("  ++ show x ++ ", " ++ show y ++ ")"
  show (EJacob x y) = "EJacob(" ++ show x ++ ", " ++ show y ++ ")"

dot :: Expr a -> Expr b -> Expr (Dot a b)
dot x y = EDot x y

sym :: String -> Expr DIM0
sym = ESym Z

vsym :: Int -> String -> Expr DIM1
vsym k = ESym (Z :. k)

msym :: (Int,Int) -> String -> Expr DIM2
msym (r,c) = ESym (Z :. r :. c)
