-- Apply.hs

{-# OPTIONS_GHC -Wall #-}

module Numeric.Dvda.Expr.Apply( evaluate
                              , getSyms
                              , substitute
                              , substitutes
                              ) where

import Data.List(foldl')

import Numeric.Dvda.Expr.Expr
import Numeric.Dvda.Expr.SourceType
import Numeric.Dvda.Expr.ElemwiseType
import Numeric.Dvda.Expr.Op2Type


-- get all the symbolic variables
getSyms :: Expr a -> [Expr a]
getSyms = getSyms' []
  where
    getSyms' acc (Op2 {arg1=x, arg2=y}) = getSyms' (getSyms' acc x) y
    getSyms' acc (Elemwise {arg=x}) = getSyms' acc x
    getSyms' acc sym@(Source {sourceType = (Sym _)}) = acc++[sym]
    getSyms' acc (Source {}) = acc


-- traverse an expression and apply a function on the sources
mapSources :: Eq a => (Expr a -> Expr b) -> Expr a -> Expr b
mapSources f op2@(Op2 {}) = Op2 { op2Type = (op2Type op2)
                                , arg1 = mapSources f (arg1 op2) 
                                , arg2 = mapSources f (arg2 op2)
                                , dim = dim op2
                                }
mapSources f ew@(Elemwise {}) = Elemwise { elemwiseType = elemwiseType ew
                                         , arg = mapSources f (arg ew)
                                         , dim = dim ew
                                         }
mapSources f src@(Source {}) = f src


-- substitute one symbolic variable for another
substitute :: Eq a => Expr a -> (Expr a, Expr a) -> Expr a
substitute oldExpr (oldValue, newValue) = mapSources f oldExpr
  where
    f src@(Source {sourceType = Sym _})
      | src == oldValue = newValue
      | otherwise       = src
    f src = src


-- substitute list of symbolic variable
substitutes :: Eq a => Expr a -> [(Expr a,Expr a)] -> Expr a
substitutes oldExpr subPairs = foldl' substitute oldExpr subPairs


-- evaluate all operations
evaluate :: Floating a => Expr a -> a
evaluate op2@(Op2 {}) = applyOp2 (op2Type op2) (evaluate (arg1 op2)) (evaluate (arg2 op2))
evaluate ew@(Elemwise {}) = applyElemwise (elemwiseType ew) (evaluate (arg ew))
evaluate (Source {sourceType = st}) = getSource st
