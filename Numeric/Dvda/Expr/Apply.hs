-- Apply.hs

{-# OPTIONS_GHC -Wall #-}

module Numeric.Dvda.Expr.Apply( getSyms
                              , substitute
                              , substitutes
                              ) where

import Data.List(nub)
import Data.Maybe

import Numeric.Dvda.Expr.Expr

-- get all the symbolic variables
getSyms :: Eq a => Expr a -> [Expr a]
getSyms expr = nub $ getSyms' [] expr
  where
    getSyms' acc x@(Sym _ _) = acc++[x]
    getSyms' acc (EScalar _) = acc
    getSyms' acc (Vector _ _) = acc
    getSyms' acc (Matrix _ _) = acc
    getSyms' acc (Unary _ x) = getSyms' acc x
    getSyms' acc (Binary _ x y) = getSyms' (getSyms' acc x) y
    getSyms' acc (Broadcast _ _) = acc

-- traverse an expression and apply a function on the sources
substitutes :: Eq a => [(Expr a, Expr a)] -> Expr a -> Expr a
substitutes subRules expr0 = substitutes' expr0
  where
    substitutes' x@(Sym _ _)
      | isNothing newSym = x
      | otherwise        = fromJust newSym
      where
        newSym = lookup x subRules
    substitutes' x@(EScalar _) = x
    substitutes' x@(Vector _ _) = x
    substitutes' x@(Matrix _ _) = x
    substitutes' (Unary op x) = Unary op $ substitutes' x
    substitutes' (Binary op x y) = Binary op (substitutes' x) (substitutes' y)
    substitutes' (Broadcast d x) = Broadcast d (substitutes' x)

substitute :: Eq a => (Expr a, Expr a) -> Expr a -> Expr a
substitute x = substitutes [x]
