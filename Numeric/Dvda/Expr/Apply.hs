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
import Numeric.Dvda.Expr.UnaryType
import Numeric.Dvda.Expr.BinaryType


-- get all the symbolic variables
getSyms :: Expr a -> [Expr a]
getSyms = getSyms' []
  where
    getSyms' acc (Binary {arg1=x, arg2=y}) = getSyms' (getSyms' acc x) y
    getSyms' acc (Unary {arg=x}) = getSyms' acc x
    getSyms' acc sym@(Source {sourceType = (Sym _)}) = acc++[sym]
    getSyms' acc (Source {}) = acc


-- traverse an expression and apply a function on the sources
mapSources :: Eq a => (Expr a -> Expr b) -> Expr a -> Expr b
mapSources f binary@(Binary {}) = Binary { binaryType = (binaryType binary)
                                         , arg1 = mapSources f (arg1 binary) 
                                         , arg2 = mapSources f (arg2 binary)
                                         , dim = dim binary
                                         }
mapSources f ew@(Unary {}) = Unary { unaryType = unaryType ew
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
evaluate binary@(Binary {}) = applyBinary (binaryType binary) (evaluate (arg1 binary)) (evaluate (arg2 binary))
evaluate ew@(Unary {}) = applyUnary (unaryType ew) (evaluate (arg ew))
evaluate (Source {sourceType = st}) = getSource st
