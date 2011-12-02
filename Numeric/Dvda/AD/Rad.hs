-- Rad.hs

{-# OPTIONS_GHC -Wall #-}

module Numeric.Dvda.AD.Rad( rad
                          , getSensitivities
                          ) where

import Numeric.Dvda.AD.Dual
import Numeric.Dvda.Internal.Expr
import Numeric.Dvda.Internal.Binary
import Numeric.Dvda.Internal.Unary
import Numeric.Dvda.Internal.Tensor

-- | Take the gradient of an expression with respect to a list of inputs
rad :: Floating a => Expr a -> [Expr a] -> [Expr a]
rad expr args = map getXSens args
  where
    senss = getSensitivities expr 1
    getXSens x = sum $ map snd $ filter (\y -> x == fst y) senss


pert :: Dual a -> a
pert (Dual _ b) = b


getSensitivities :: Floating a => Expr a -> Expr a -> [(Expr a, Expr a)]
getSensitivities primal@(EScalar (TSym _ _)) sens = [(primal, sens)]
getSensitivities primal@(EVector (TSym _ _)) sens = [(primal, sens)]
getSensitivities primal@(EMatrix (TSym _ _)) sens = [(primal, sens)]
getSensitivities (EScalar (TUnary (Unary unType x'))) sens = getSensitivities x (sens*dfdx)
  where
    dfdx = pert $ applyUnary unType (Dual x 1)
    x = EScalar x'
getSensitivities (EVector (TUnary (Unary unType x'))) sens = getSensitivities x (sens*dfdx)
  where
    dfdx = pert $ applyUnary unType (Dual x 1)
    x = EVector x'
getSensitivities (EMatrix (TUnary (Unary unType x'))) sens = getSensitivities x (sens*dfdx)
  where
    dfdx = pert $ applyUnary unType (Dual x 1)
    x = EMatrix x'
getSensitivities (EScalar (TBinary (Binary binType x' y'))) sens = getSensitivities x (sens*dfdx) ++
                                                                   getSensitivities y (sens*dfdy)
  where
    dfdx = pert $ applyBinary binType (Dual x 1) (Dual y 0)
    dfdy = pert $ applyBinary binType (Dual x 0) (Dual y 1)
    x = EScalar x'
    y = EScalar y'
getSensitivities (EVector (TBinary (Binary binType x' y'))) sens = getSensitivities x (sens*dfdx) ++
                                                                   getSensitivities y (sens*dfdy)
  where
    dfdx = pert $ applyBinary binType (Dual x 1) (Dual y 0)
    dfdy = pert $ applyBinary binType (Dual x 0) (Dual y 1)
    x = EVector x'
    y = EVector y'
getSensitivities (EMatrix (TBinary (Binary binType x' y'))) sens = getSensitivities x (sens*dfdx) ++
                                                                   getSensitivities y (sens*dfdy)
  where
    dfdx = pert $ applyBinary binType (Dual x 1) (Dual y 0)
    dfdy = pert $ applyBinary binType (Dual x 0) (Dual y 1)
    x = EMatrix x'
    y = EMatrix y'
getSensitivities _ _ = []
