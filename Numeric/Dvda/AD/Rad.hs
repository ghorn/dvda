-- Rad.hs

{-# OPTIONS_GHC -Wall #-}

module Numeric.Dvda.AD.Rad( rad
                          , getSensitivities
                          ) where

import Numeric.Dvda.AD.Dual
import Numeric.Dvda.Expr
import Numeric.Dvda.Expr.Scalar
import Numeric.Dvda.Expr.Vector
import Numeric.Dvda.Expr.Matrix

rad :: Floating a => Expr a -> [Expr a] -> [Expr a]
rad expr args = map getXSens args
  where
    senss = getSensitivities expr 1
    getXSens x = sum $ map snd $ filter (\y -> x == fst y) senss


pert :: Dual a -> a
pert (Dual _ b) = b


getSensitivities :: Floating a => Expr a -> Expr a -> [(Expr a, Expr a)]
getSensitivities primal@(EScalar (SSym _)) sens = [(primal, sens)]
getSensitivities primal@(EVector (VSym _ _)) sens = [(primal, sens)]
getSensitivities primal@(EMatrix (MSym _ _)) sens = [(primal, sens)]
getSensitivities (EScalar (SUnary (Unary unType x'))) sens = getSensitivities x (sens*dfdx)
  where
    dfdx = pert $ applyUnary unType (Dual x 1)
    x = EScalar x'
getSensitivities (EVector (VUnary (Unary unType x'))) sens = getSensitivities x (sens*dfdx)
  where
    dfdx = pert $ applyUnary unType (Dual x 1)
    x = EVector x'
getSensitivities (EMatrix (MUnary (Unary unType x'))) sens = getSensitivities x (sens*dfdx)
  where
    dfdx = pert $ applyUnary unType (Dual x 1)
    x = EMatrix x'
getSensitivities (EScalar (SBinary (Binary binType x' y'))) sens = (getSensitivities x (sens*dfdx))++
                                                                   (getSensitivities y (sens*dfdy))
  where
    dfdx = pert $ applyBinary binType (Dual x 1) (Dual y 0)
    dfdy = pert $ applyBinary binType (Dual x 0) (Dual y 1)
    x = EScalar x'
    y = EScalar y'
getSensitivities (EVector (VBinary (Binary binType x' y'))) sens = (getSensitivities x (sens*dfdx))++
                                                                   (getSensitivities y (sens*dfdy))
  where
    dfdx = pert $ applyBinary binType (Dual x 1) (Dual y 0)
    dfdy = pert $ applyBinary binType (Dual x 0) (Dual y 1)
    x = EVector x'
    y = EVector y'
getSensitivities (EMatrix (MBinary (Binary binType x' y'))) sens = (getSensitivities x (sens*dfdx))++
                                                                   (getSensitivities y (sens*dfdy))
  where
    dfdx = pert $ applyBinary binType (Dual x 1) (Dual y 0)
    dfdy = pert $ applyBinary binType (Dual x 0) (Dual y 1)
    x = EMatrix x'
    y = EMatrix y'
getSensitivities _ _ = []
