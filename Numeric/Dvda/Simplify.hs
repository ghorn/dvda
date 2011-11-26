-- Simplify.hs

{-# OPTIONS_GHC -Wall #-}

module Numeric.Dvda.Simplify( pruneZeros
                            , pruneZerosOnce
                            , fastSimplify
                            , removeTimesOne
                            ) where

import Numeric.Dvda.Expr.Expr
import Numeric.Dvda.Expr.Op2Type
import Numeric.Dvda.Expr.ElemwiseType
import Numeric.Dvda.Expr.SourceType

fastSimplify :: Num a => Expr a -> Expr a
fastSimplify = removeTimesOne . pruneZeros

-- only need to be called once
removeTimesOne :: Expr a -> Expr a
removeTimesOne (Op2 {op2Type = Mul, arg1 = Source {sourceType = I 1, dim = 0}, arg2 = y}) = removeTimesOne y
removeTimesOne (Op2 {op2Type = Mul, arg2 = Source {sourceType = I 1, dim = 0}, arg1 = x}) = removeTimesOne x
removeTimesOne op2@(Op2 {}) = Op2 {op2Type = op2Type op2
                                  , arg1 = removeTimesOne (arg1 op2)
                                  , arg2 = removeTimesOne (arg2 op2)
                                  , dim = dim op2
                                  }
removeTimesOne ew@(Elemwise {}) = Elemwise { elemwiseType = elemwiseType ew
                                           , arg = removeTimesOne (arg ew)
                                           , dim = dim ew
                                           }
removeTimesOne src@(Source {}) = src

pruneZeros :: Num a => Expr a -> Expr a
pruneZeros x 
  | x == xPruned = x
  | otherwise    = pruneZeros xPruned
  where
    xPruned = pruneZerosOnce x



pruneZerosOnce :: Num a => Expr a -> Expr a
-- source:
pruneZerosOnce src@(Source {}) = src
-- elementwise:
pruneZerosOnce (Elemwise { elemwiseType = Abs
                         , arg = Source {sourceType = (I 0), dim = dim'}}) = Source {sourceType = I 0, dim = dim'}
pruneZerosOnce (Elemwise { elemwiseType = Signum
                         , arg = Source {sourceType = (I 0), dim = dim'}}) = Source {sourceType = I 0, dim = dim'}
pruneZerosOnce (Elemwise { elemwiseType = Neg
                         , arg = Source {sourceType = (I 0), dim = dim'}}) = Source {sourceType = I 0, dim = dim'}
pruneZerosOnce (Elemwise {elemwiseType = ewt
                         , arg = x, dim = dim'}) = Elemwise {elemwiseType = ewt, arg = pruneZeros x, dim = dim'}
-- op2
pruneZerosOnce (Op2 {op2Type = Mul, arg1 = Source {sourceType = I 0}, dim = 0}) = Source { sourceType = I 0, dim = 0 }
pruneZerosOnce (Op2 {op2Type = Mul, arg2 = Source {sourceType = I 0}, dim = 0}) = Source { sourceType = I 0, dim = 0 }
pruneZerosOnce op2@(Op2 {op2Type = Mul}) = Op2 { op2Type = Mul
                                              , arg1 = pruneZerosOnce (arg1 op2)
                                              , arg2 = pruneZerosOnce (arg2 op2)
                                              , dim = dim op2}
pruneZerosOnce (Op2 {op2Type = Div, arg1 = Source {sourceType = I 0}, dim = 0}) = Source { sourceType = I 0, dim = 0 }
--pruneZerosOnce (Op2 {op2Type = Div, arg2 = Source {sourceType = I 0}, dim = 0}) = error "divide by zero in pruneZerosOnce Div"
pruneZerosOnce op2@(Op2 {op2Type = Div}) = Op2 { op2Type = Div
                                               , arg1 = pruneZerosOnce (arg1 op2)
                                               , arg2 = pruneZerosOnce (arg2 op2)
                                               , dim = dim op2}
pruneZerosOnce (Op2 {op2Type = Add, arg1 = (Source {sourceType = I 0}), arg2 = y, dim = 0}) = pruneZeros y
pruneZerosOnce (Op2 {op2Type = Add, arg2 = (Source {sourceType = I 0}), arg1 = x, dim = 0}) = pruneZeros x
pruneZerosOnce op2@(Op2 {op2Type = Add}) = Op2 { op2Type = Add
                                              , arg1 = pruneZerosOnce (arg1 op2)
                                              , arg2 = pruneZerosOnce (arg2 op2)
                                              , dim = dim op2}
pruneZerosOnce (Op2 {op2Type = Sub, arg1 = (Source {sourceType = I 0}), arg2 = y, dim = 0}) = negate $ pruneZeros y
pruneZerosOnce (Op2 {op2Type = Sub, arg2 = (Source {sourceType = I 0}), arg1 = x, dim = 0}) = pruneZeros x
pruneZerosOnce op2@(Op2 {op2Type = Sub}) = Op2 { op2Type = Sub
                                              , arg1 = pruneZerosOnce (arg1 op2)
                                              , arg2 = pruneZerosOnce (arg2 op2)
                                              , dim = dim op2}
pruneZerosOnce op2@(Op2 {op2Type = Pow}) = Op2 { op2Type = Pow
                                              , arg1 = pruneZerosOnce (arg1 op2)
                                              , arg2 = pruneZerosOnce (arg2 op2)
                                              , dim = dim op2}
pruneZerosOnce op2@(Op2 {op2Type = LogBase}) = Op2 { op2Type = LogBase
                                              , arg1 = pruneZerosOnce (arg1 op2)
                                              , arg2 = pruneZerosOnce (arg2 op2)
                                              , dim = dim op2}
