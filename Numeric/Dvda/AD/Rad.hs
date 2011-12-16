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
getSensitivities primal@(Expr (TSym _ _)) sens = [(primal, sens)]
getSensitivities (Expr (TUnary (Unary unType g'))) sens = getSensitivities g (sens*dfdg)
  where
    dfdg = pert $ applyUnary unType (Dual g 1)
    g = Expr g'
getSensitivities (Expr (TBinary (Binary binType g' h'))) sens = getSensitivities g (sens*dfdg) ++
                                                                   getSensitivities h (sens*dfdh)
  where
    dfdg = pert $ applyBinary binType (Dual g 1) (Dual h 0)
    dfdh = pert $ applyBinary binType (Dual g 0) (Dual h 1)
    g = Expr g'
    h = Expr h'
getSensitivities _ _ = []
