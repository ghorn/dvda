{- |
   Module      : Numeric.Dvda.Symbolic
   Description : Basic symbolic expression api

   `Numeric.Dvda.Expr.Expr` is the symbolic expression type which is used to access all Dvda functions.
   This module provides the API for creating symbolic and numeric `Numeric.Dvda.Expr.Expr`s.
 -}

{-# OPTIONS_GHC -Wall #-}

module Numeric.Dvda.Symbolic( -- * Symbolic expressions
                              sym
                            , symVec
                            , symMat
                              -- * Numeric expressions
                            , sca
                            , vec
                            , mat
                              -- * Evaluation
                            , eval
                            , evalE
                              -- * Substitution
                            , subs
                              -- * Dimensions
                            , dim
                            ) where

import Data.List(nubBy)
import Data.Maybe(isNothing, fromJust)

import Numeric.Dvda.Internal.Expr
import Numeric.Dvda.Internal.Tensor
import Numeric.Dvda.Internal.Binary
import Numeric.Dvda.Internal.Unary

-- | Get the dimensions of an expression. Scalar: [], vector [r], matrix: [r,c]
dim :: Expr a -> [Int]
dim (Expr x) = tDim x

-- | Evalute numeric operations in expression returning numereric `Numeric.Dvda.Expr.Expr` form
--   .
--   Causes exception if Expression has any symbolic variables
evalE :: Floating a => Expr a -> Expr a
evalE (Expr x) = Expr $ TNum (tDim x) (tEval x)

-- | Evalute numeric operations in expression returning dimensions and "array"
--   .
--   Causes exception if Expression has any symbolic variables
eval :: Floating a => Expr a -> ([Int],[a])
eval (Expr x) = (tDim x, tEval x)

-- | create symbolic scalar
sym :: String -> Expr a
sym name = Expr $ TSym [] name

-- | create symbolic vector with length
symVec :: Int -> String -> Expr a
symVec d name 
  | d > 0     = Expr $ TSym [d] name
  | otherwise = error $ "symVec can't make vector with length: " ++ show d

-- | create symbolic matrix with specified (rows, columns)
symMat :: (Int,Int) -> String -> Expr a
symMat (r,c) name
  | r > 0 && c > 0 = Expr $ TSym [r,c] name
  | otherwise      = error $ "symMat can't make matrix with dimensions: " ++ show (r,c)

-- | explicitly convert a numeric value to an expression
sca :: a -> Expr a
sca x = Expr $ TNum [] [x]

-- | create numeric vector
vec :: [a] -> Expr a
vec xs 
  | length xs > 0 = Expr $ TNum [length xs] xs
  | otherwise     = error "Improper dimensions in vec :: [a] -> Expr a"

-- | Create numeric matrix with specified (rows, cols). List is taken rowwise.
mat :: (Int,Int) -> [a] -> Expr a
mat (r,c) xs 
  | (length xs == r*c) && (r > 0) && (c > 0) = Expr $ TNum [r,c] xs
  | otherwise                                = error "Improper dimensions in mat :: (Int,Int) -> [a] -> Expr a"


-- | substitute a list of pairs [(matchThis, replaceWithThis)] in an expression
subs :: Floating a => [(Expr a, Expr a)] -> Expr a -> Expr a
subs subslist (Expr tensor)
  | length subslist == length (nubBy contradictingSub subslist) = subs' tensor
  | otherwise = error "Error in subs: same input in substitute pairs list twice with different outputs"
  where
    contradictingSub (x0,x1) (y0,y1) = (x0 == y0) && (x1 /= y1)
    
    tSubslist = map (\(Expr x, Expr y) -> (x, y)) subslist

    subs' x@(TNum _ _) = Expr x
    subs' x@(TInt _ _) = Expr x
    subs' x@(TSym _ _) = Expr $ sub tSubslist x
    subs' (TUnary (Unary unOp x)) = applyUnary unOp $ subs' x
    subs' (TBinary (Binary binOp x y)) = applyBinary binOp (subs' x) (subs' y)
    subs' (TBroadcast [] _) = error "api fail in subs' in subs"
    subs' (TBroadcast _ x) = subs' x

    sub :: Eq a => [(a,a)] -> a -> a
    sub sublist arg 
      | isNothing newSym = arg
      | otherwise        = fromJust newSym
      where
        newSym = lookup arg sublist
