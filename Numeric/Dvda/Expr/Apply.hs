-- Apply.hs

{-# OPTIONS_GHC -Wall #-}

module Numeric.Dvda.Expr.Apply( evaluate
                              , getSyms
                              , mapSources
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
    getSyms' acc (Op2 _ x y) = getSyms' (getSyms' acc x) y
    getSyms' acc (Elemwise _ x) = getSyms' acc x
    getSyms' acc sym@(Source (Sym _)) = acc++[sym]
    getSyms' acc (Source _) = acc


-- traverse an expression and apply a function on the sources
mapSources :: Eq a => (Expr a -> Expr b) -> Expr a -> Expr b
mapSources f (Op2 op2t x y) = Op2 op2t (mapSources f x) (mapSources f y)
mapSources f (Elemwise ewt x) = Elemwise ewt (mapSources f x)
mapSources f src@(Source _) = f src


-- substitute one symbolic variable for another
substitute :: Eq a => Expr a -> (Expr a, Expr a) -> Expr a
substitute oldExpr (oldValue, newValue) = mapSources f oldExpr
  where
    f src@(Source (Sym _))
      | src == oldValue = newValue
      | otherwise       = src
    f src = src


-- substitute list of symbolic variable
substitutes :: Eq a => Expr a -> [(Expr a,Expr a)] -> Expr a
substitutes oldExpr subPairs = foldl' substitute oldExpr subPairs


-- evaluate all operations
evaluate :: Floating a => Expr a -> a
evaluate (Op2 op2t x y) = applyOp2 op2t (evaluate x) (evaluate y)
evaluate (Elemwise ewt x) = applyElemwise ewt (evaluate x)
evaluate (Source st) = getSource st
