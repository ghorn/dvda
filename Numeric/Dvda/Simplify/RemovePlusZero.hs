-- RemovePlusZero.hs

{-# OPTIONS_GHC -Wall #-}

module Numeric.Dvda.Simplify.RemovePlusZero( removePlusZero
                                           ) where

import Numeric.Dvda.Expr

-- may need to be called more than once
removePlusZero :: Num a => Expr a -> Expr a
removePlusZero src@(Source {}) = src
removePlusZero bin@(Binary {})
  | binaryType bin == Add && isZero (arg1 bin) = removePlusZero (arg2 bin)
  | binaryType bin == Add && isZero (arg2 bin) = removePlusZero (arg1 bin)
  | binaryType bin == Sub && isZero (arg1 bin) = negate $ removePlusZero (arg2 bin)
  | binaryType bin == Sub && isZero (arg2 bin) = removePlusZero (arg1 bin)
  | otherwise = Binary {binaryType = binaryType bin, arg1 = removePlusZero (arg1 bin), arg2 = removePlusZero (arg2 bin), dim = dim bin}
removePlusZero ew@(Unary {}) = Unary { unaryType = unaryType ew
                                     , arg = removePlusZero (arg ew)
                                     , dim = dim ew
                                     }

isZero :: Expr a -> Bool
isZero (Source {sourceType = I 0}) = True
isZero (Source {}) = False
isZero ew@(Unary {})
  | unaryType ew `elem` [Broadcast, Neg, Abs] = isZero $ arg ew
  | otherwise = False
isZero (Binary {}) = False
