-- Rad.hs

{-# OPTIONS_GHC -Wall #-}

module Numeric.Dvda.AD.Rad( rad
                          , getSensitivities
                          ) where

import Numeric.Dvda.Expr.Expr
import Numeric.Dvda.Expr.Op2Type
import Numeric.Dvda.Expr.ElemwiseType
import Numeric.Dvda.Expr.SourceType
import Numeric.Dvda.Simplify
import Numeric.Dvda.AD.Dual

rad :: Floating a => Expr a -> [Expr a] -> [Expr a]
rad expr args = map getXSens args
  where
    senss = getSensitivities expr 1
    getXSens x = fastSimplify $ sum $ map snd $ filter (\y -> x == fst y) senss


pert :: Dual a -> a
pert (Dual _ b) = b


getSensitivities :: Floating a => Expr a -> Expr a -> [(Expr a, Expr a)]
getSensitivities primal@(Source {sourceType =Sym _}) sens = [(primal, fastSimplify sens)]
getSensitivities (Source {}) _ = []
getSensitivities (Op2 { op2Type = op2t 
                      , arg1 = x 
                      , arg2 = y}) sens = (getSensitivities x (sens*dfdx))++
                                          (getSensitivities y (sens*dfdy))
  where
    f = applyOp2 op2t
    dfdx = pert $ f (Dual x 1) (Dual y 0)
    dfdy = pert $ f (Dual x 0) (Dual y 1)
getSensitivities (Elemwise {elemwiseType = ewt, arg = x}) sens = getSensitivities x (sens*dfdx)
  where
    f = applyElemwise ewt
    dfdx = pert $ f (Dual x 1)
