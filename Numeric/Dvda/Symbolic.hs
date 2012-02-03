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
import Numeric.Dvda.Internal.BinaryType
import Numeric.Dvda.Internal.UnaryType
import Numeric.Dvda.Dim

-- | Get the dimensions of an expression. Scalar: [], vector [r], matrix: [r,c]
dim :: Expr a -> Dim
dim (Expr x) = tDim x

-- | Evalute numeric operations in expression returning numereric `Numeric.Dvda.Expr.Expr` form
--   .
--   Causes exception if Expression has any symbolic variables
evalE :: Floating a => Expr a -> Expr a
evalE (Expr x) = Expr $ TNum (tDim x) (tEval x)

-- | Evalute numeric operations in expression returning dimensions and "array"
--   .
--   Causes exception if Expression has any symbolic variables
eval :: Floating a => Expr a -> (Dim,[a])
eval (Expr x) = (tDim x, tEval x)

-- | create symbolic scalar
sym :: String -> Expr a
sym name = Expr $ TSym D0 name

-- | create symbolic vector with length
symVec :: Show a => Int -> String -> Expr a
symVec d name 
  | d > 0     = Expr $ TSym (Dim [d]) name
  | otherwise = error $ "symVec can't make vector with length: " ++ show d

-- | create symbolic matrix with specified (rows, columns)
symMat :: (Int,Int) -> String -> Expr a
symMat (r,c) name
  | r > 0 && c > 0 = Expr $ TSym (Dim [r,c]) name
  | otherwise      = error $ "symMat can't make matrix with dimensions: " ++ show (r,c)

-- | explicitly convert a numeric value to an expression
sca :: a -> Expr a
sca x = Expr $ TNum D0 [x]

-- | create numeric vector
vec :: [a] -> Expr a
vec xs 
  | length xs > 0 = Expr $ TNum (Dim [length xs]) xs
  | otherwise     = error "Improper dimensions in vec :: [a] -> Expr a"

-- | Create numeric matrix with specified (rows, cols). List is taken rowwise.
mat :: (Int,Int) -> [a] -> Expr a
mat (r,c) xs 
  | (length xs == r*c) && (r > 0) && (c > 0) = Expr $ TNum (Dim [r,c]) xs
  | otherwise                                = error "Improper dimensions in mat :: (Int,Int) -> [a] -> Expr a"


-- | substitute a list of pairs [(matchThis, replaceWithThis)] in an expression
subs :: (Floating a, Eq a, Show a) => [(Expr a, Expr a)] -> Expr a -> Expr a
subs subslist (Expr tensor)
  | length subslist == length (nubBy contradictingSub subslist) = subs' tensor
  | otherwise = error "Error in subs: same input in substitute pairs list twice with different outputs"
  where
    contradictingSub (x0,x1) (y0,y1) = (x0 == y0) && (x1 /= y1)
    
    tSubslist = map (\(Expr x, Expr y) -> (x, y)) subslist

    subs' x@(TNum _ _) = Expr x
    subs' x@(TInt _ _) = Expr x
    subs' x@(TSym _ _) = Expr $ sub tSubslist x
    subs' (TUnary unOp x) = applyUnary unOp $ subs' x
    subs' (TBinary binOp x y) = applyBinary binOp (subs' x) (subs' y)
    subs' (TBroadcast D0 _) = error "api fail in subs' in subs"
    subs' (TBroadcast _ x) = subs' x

    sub :: Eq a => [(a,a)] -> a -> a
    sub sublist arg 
      | isNothing newSym = arg
      | otherwise        = fromJust newSym
      where
        newSym = lookup arg sublist
