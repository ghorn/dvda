-- RemoveTimesOne.hs

{-# OPTIONS_GHC -Wall #-}

module Numeric.Dvda.Simplify.RemoveTimesOne( removeTimesOne
                                           ) where

import Numeric.Dvda.Expr

-- may need to be called more than once
removeTimesOne :: Expr a -> Expr a
removeTimesOne src@(Source {}) = src
removeTimesOne bin@(Binary {})
  | binaryType bin == Mul && isOne (arg1 bin) = removeTimesOne (arg2 bin)
  | binaryType bin == Mul && isOne (arg2 bin) = removeTimesOne (arg1 bin)
  | otherwise = Binary {binaryType = binaryType bin, arg1 = removeTimesOne (arg1 bin), arg2 = removeTimesOne (arg2 bin), dim = dim bin}
removeTimesOne ew@(Unary {}) = Unary { unaryType = unaryType ew
                                     , arg = removeTimesOne (arg ew)
                                     , dim = dim ew
                                     }

isOne :: Expr a -> Bool
isOne (Source {sourceType = I 1}) = True
isOne (Source {}) = False
isOne ew@(Unary {unaryType = Broadcast}) = isOne $ arg ew
isOne (Unary {}) = False
isOne (Binary {}) = False
