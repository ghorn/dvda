{- |
   Module      : Numeric.Dvda.Symbolic
   Description : Basic symbolic expression api

   `Numeric.Dvda.Expr.Expr` is the symbolic expression type which is used to access all Dvda functions.
   This module provides the API for creating symbolic and numeric `Numeric.Dvda.Expr.Expr`s.
 -}

{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE Rank2Types #-}

module Numeric.Dvda.Symbolic( sym
                            , symVec
                            , symMat
                            , sca
                            , vec
                            , mat
                            , eval
                            , subs
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
dim (EScalar x) = tDim x
dim (EVector x) = tDim x
dim (EMatrix x) = tDim x

-- | evalute expression down to numeric values
eval :: Floating a => Expr a -> Expr a
eval (EScalar x) = EScalar $ TNum [] (snd $ tEval x)
eval (EVector x) = EVector $ TNum d v
  where
    (d, v) = tEval x
eval (EMatrix x) = EMatrix $ TNum d m
  where
    (d, m) = tEval x

-- | create symbolic scalar
sym :: String -> Expr a
sym name = EScalar $ TSym [] name

-- | create symbolic vector with length
symVec :: Int -> String -> Expr a
symVec d name 
  | d > 0     = EVector $ TSym [d] name
  | otherwise = error $ "symVec can't make vector with length: " ++ show d

-- | create symbolic matrix with specified (rows, columns)
symMat :: (Int,Int) -> String -> Expr a
symMat (r,c) name
  | r > 0 && c > 0 = EMatrix $ TSym [r,c] name
  | otherwise      = error $ "symMat can't make matrix with dimensions: " ++ show (r,c)

-- | explicitely convert a numeric value to an expression
sca :: a -> Expr a
sca x = EScalar $ TNum [] [x]

-- | create numeric vector
vec :: [a] -> Expr a
vec xs 
  | length xs > 0 = EVector $ TNum [length xs] xs
  | otherwise     = error "Improper dimensions in vec :: [a] -> Expr a"

-- | Create numeric matrix with specified (rows, cols). List is taken rowwise.
mat :: (Int,Int) -> [a] -> Expr a
mat (r,c) xs 
  | (length xs == r*c) && (r > 0) && (c > 0) = EMatrix $ TNum [r,c] xs
  | otherwise                                = error "Improper dimensions in mat :: (Int,Int) -> [a] -> Expr a"


-- | substitute a list of pairs [(matchThis, replaceWithThis)] in an expression
subs :: Floating a => [(Expr a, Expr a)] -> Expr a -> Expr a
subs subslist expr
  | length subslist == length (nubBy contradictingSub subslist) = callCorrectSub expr
  | otherwise = error "Error in subs: same input in substitute pairs list twice with different outputs"
  where
    contradictingSub (x0,x1) (y0,y1) = (x0 == y0) && (x1 /= y1)
    
    callCorrectSub (EScalar s) = sSubs s
    callCorrectSub (EVector v) = vSubs v
    callCorrectSub (EMatrix m) = mSubs m
    
    (scalarSubs, vectorSubs, matrixSubs) = foldr sortSubs ([],[],[]) subslist
    
    sortSubs (EScalar x, EScalar y) (ss,vs,ms) = ( ss ++ [(x,y)], vs           , ms            )
    sortSubs (EVector x, EVector y) (ss,vs,ms) = ( ss           , vs ++ [(x,y)], ms            )
    sortSubs (EMatrix x, EMatrix y) (ss,vs,ms) = ( ss           , vs           , ms ++ [(x,y)] )
    sortSubs xy _ = error $ "Can't substitute two unlike quantities, must be scalar -> scalar, vector -> vector, or matrix -> matrix)\noffending pair: " ++ show xy

    sSubs x@(TNum _ _) = EScalar x
    sSubs x@(TInt _ _) = EScalar x
    sSubs x@(TSym _ _) = EScalar $ sub scalarSubs x
    sSubs (TUnary (Unary unOp x)) = applyUnary unOp $ sSubs x
    sSubs (TBinary (Binary binOp x y)) = applyBinary binOp (sSubs x) (sSubs y)
    sSubs (TBroadcast _ _) = error "api fail in sSubs in subs"
    
    vSubs x@(TNum _ _) = EVector x
    vSubs x@(TInt _ _) = EVector x
    vSubs x@(TSym _ _) = EVector $ sub vectorSubs x
    vSubs (TUnary (Unary unOp x)) = applyUnary unOp (vSubs x)
    vSubs (TBinary (Binary binOp x y)) = applyBinary binOp (vSubs x) (vSubs y)
    vSubs (TBroadcast _ x) = sSubs x
    
    mSubs x@(TNum _ _) = EMatrix x
    mSubs x@(TInt _ _) = EMatrix x
    mSubs x@(TSym _ _) = EMatrix $ sub matrixSubs x
    mSubs (TUnary (Unary unOp x)) = applyUnary unOp $ mSubs x
    mSubs (TBinary (Binary binOp x y)) = applyBinary binOp (mSubs x) (mSubs y)
    mSubs (TBroadcast _ x) = sSubs x

    sub :: Eq a => [(a,a)] -> a -> a
    sub sublist arg 
      | isNothing newSym = arg
      | otherwise        = fromJust newSym
      where
        newSym = lookup arg sublist
