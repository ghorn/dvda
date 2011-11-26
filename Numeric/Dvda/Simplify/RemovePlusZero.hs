-- RemovePlusZero.hs

{-# OPTIONS_GHC -Wall #-}

module Numeric.Dvda.Simplify.RemovePlusZero( removePlusZero
                                           ) where

import Numeric.Dvda.Expr

-- may need to be called more than once
removePlusZero :: Num a => Expr a -> Expr a
removePlusZero src@(Source {}) = src
removePlusZero op2@(Op2 {})
  | op2Type op2 == Add && isZero (arg1 op2) = removePlusZero (arg2 op2)
  | op2Type op2 == Add && isZero (arg2 op2) = removePlusZero (arg1 op2)
  | op2Type op2 == Sub && isZero (arg1 op2) = negate $ removePlusZero (arg2 op2)
  | op2Type op2 == Sub && isZero (arg2 op2) = removePlusZero (arg1 op2)
  | otherwise = Op2 {op2Type = op2Type op2, arg1 = removePlusZero (arg1 op2), arg2 = removePlusZero (arg2 op2), dim = dim op2}
removePlusZero ew@(Elemwise {}) = Elemwise { elemwiseType = elemwiseType ew
                                           , arg = removePlusZero (arg ew)
                                           , dim = dim ew
                                           }

isZero :: Expr a -> Bool
isZero (Source {sourceType = I 0}) = True
isZero (Source {}) = False
isZero ew@(Elemwise {})
  | elemwiseType ew `elem` [Broadcast, Neg, Abs] = isZero $ arg ew
  | otherwise = False
isZero (Op2 {}) = False
