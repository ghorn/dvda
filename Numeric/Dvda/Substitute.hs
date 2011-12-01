-- Substitute.hs

{-# OPTIONS_GHC -Wall #-}

module Numeric.Dvda.Substitute( subs
                              ) where


import Data.List(nubBy)
import Data.Maybe(isNothing, fromJust)

import Numeric.Dvda.Expr
import Numeric.Dvda.Internal.Binary
import Numeric.Dvda.Internal.Unary
import Numeric.Dvda.Internal.Scalar
import Numeric.Dvda.Internal.Vector
import Numeric.Dvda.Internal.Matrix

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

    sSubs x@(SNum _) = EScalar x
    sSubs x@(SInt _) = EScalar x
    sSubs x@(SSym _) = EScalar $ sub scalarSubs x
    sSubs (SUnary (Unary unOp x)) = applyUnary unOp $ sSubs x
    sSubs (SBinary (Binary binOp x y)) = applyBinary binOp (sSubs x) (sSubs y)
    
    vSubs x@(VNum _ _) = EVector x
    vSubs x@(VSym _ _) = EVector $ sub vectorSubs x
    vSubs (VUnary (Unary unOp x)) = applyUnary unOp (vSubs x)
    vSubs (VBinary (Binary binOp x y)) = applyBinary binOp (vSubs x) (vSubs y)
    vSubs (VBroadcast _ x) = sSubs x
    
    mSubs x@(MNum _ _) = EMatrix x
    mSubs x@(MSym _ _) = EMatrix $ sub matrixSubs x
    mSubs (MUnary (Unary unOp x)) = applyUnary unOp $ mSubs x
    mSubs (MBinary (Binary binOp x y)) = applyBinary binOp (mSubs x) (mSubs y)
    mSubs (MBroadcast _ x) = sSubs x

    sub :: Eq a => [(a,a)] -> a -> a
    sub sublist arg 
      | isNothing newSym = arg
      | otherwise        = fromJust newSym
      where
        newSym = lookup arg sublist
