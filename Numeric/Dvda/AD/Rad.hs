-- Rad.hs

{-# OPTIONS_GHC -Wall #-}

module Numeric.Dvda.AD.Rad( rad
                          , getSensitivities
                          ) where

import Numeric.Dvda.AD.Dual
import Numeric.Dvda.Expr
import Numeric.Dvda.Simplify

rad :: Floating a => Expr a -> [Expr a] -> [Expr a]
rad expr args = map getXSens args
  where
    senss = getSensitivities expr 1
    getXSens x = removeIdentities $ sum $ map snd $ filter (\y -> x == fst y) senss


pert :: Dual a -> a
pert (Dual _ b) = b


getSensitivities :: Floating a => Expr a -> Expr a -> [(Expr a, Expr a)]
getSensitivities primal@(Source {sourceType =Sym _}) sens = [(primal, removeIdentities sens)]
getSensitivities (Source {}) _ = []
getSensitivities (Binary { binaryType = bint
                         , arg1 = x 
                         , arg2 = y}) sens = (getSensitivities x (sens*dfdx))++
                                             (getSensitivities y (sens*dfdy))
  where
    f = applyBinary bint
    dfdx = pert $ f (Dual x 1) (Dual y 0)
    dfdy = pert $ f (Dual x 0) (Dual y 1)
getSensitivities (Unary {unaryType = ewt, arg = x}) sens = getSensitivities x (sens*dfdx)
  where
    f = applyUnary ewt
    dfdx = pert $ f (Dual x 1)
