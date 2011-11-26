-- RemoveTimesOne.hs

{-# OPTIONS_GHC -Wall #-}

module Numeric.Dvda.Simplify.RemoveTimesOne( removeTimesOne
                                           ) where

import Numeric.Dvda.Expr

-- may need to be called more than once
removeTimesOne :: Expr a -> Expr a
removeTimesOne src@(Source {}) = src
removeTimesOne op2@(Op2 {})
  | op2Type op2 == Mul && isOne (arg1 op2) = removeTimesOne (arg2 op2)
  | op2Type op2 == Mul && isOne (arg2 op2) = removeTimesOne (arg1 op2)
  | otherwise = Op2 {op2Type = op2Type op2, arg1 = removeTimesOne (arg1 op2), arg2 = removeTimesOne (arg2 op2), dim = dim op2}
removeTimesOne ew@(Elemwise {}) = Elemwise { elemwiseType = elemwiseType ew
                                           , arg = removeTimesOne (arg ew)
                                           , dim = dim ew
                                           }

isOne :: Expr a -> Bool
isOne (Source {sourceType = I 1}) = True
isOne (Source {}) = False
isOne ew@(Elemwise {elemwiseType = Broadcast}) = isOne $ arg ew
isOne (Elemwise {}) = False
isOne (Op2 {}) = False
