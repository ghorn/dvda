-- Simplify.hs

{-# OPTIONS_GHC -Wall #-}

module Had.Simplify( pruneZeros
                   , pruneZerosOnce
                   , fastSimplify
                   , removeTimesOne
                   ) where

import Had.Expr.Expr
import Had.Expr.Op2Type
import Had.Expr.ElemwiseType
import Had.Expr.SourceType

fastSimplify :: Num a => Expr a -> Expr a
fastSimplify = removeTimesOne . pruneZeros

-- only need to be called once
removeTimesOne :: Expr a -> Expr a
removeTimesOne (Op2 Mul (Source (I 1)) x) = removeTimesOne x
removeTimesOne (Op2 Mul x (Source (I 1))) = removeTimesOne x
removeTimesOne (Op2 asm x y) = Op2 asm (removeTimesOne x) (removeTimesOne y)
removeTimesOne (Elemwise ewo x) = Elemwise ewo (removeTimesOne x)
removeTimesOne src@(Source _) = src

pruneZeros :: (Show a, Eq a, Num a) => Expr a -> Expr a
pruneZeros x 
  | x == xPruned = x
  | otherwise    = pruneZeros xPruned
  where
    xPruned = pruneZerosOnce x

pruneZerosOnce :: (Show a, Eq a, Num a) => Expr a -> Expr a
pruneZerosOnce (Op2 op2Type x y) = op2PruneZeros op2Type x y
pruneZerosOnce (Elemwise elemwiseType x) = elemwisePruneZeros elemwiseType x
pruneZerosOnce (Source x) = (Source x)

-- elemwise prune zeros
elemwisePruneZeros :: Num a => ElemwiseType -> Expr a -> Expr a
elemwisePruneZeros Abs (Source Zero) = Source Zero
elemwisePruneZeros Abs x = Elemwise Abs (pruneZeros x)
elemwisePruneZeros Signum (Source Zero) = Source Zero
elemwisePruneZeros Signum x = Elemwise Signum (pruneZeros x)
elemwisePruneZeros Negate (Source Zero) = Source Zero
elemwisePruneZeros Negate x = Elemwise Negate (pruneZeros x)

-- op2 prune zeros
op2PruneZeros :: Num a => Op2Type -> Expr a -> Expr a -> Expr a
op2PruneZeros Mul (Source Zero) _ = Source Zero
op2PruneZeros Mul _ (Source Zero) = Source Zero
op2PruneZeros Mul x y = Op2 Mul (pruneZeros x) (pruneZeros y)

op2PruneZeros Add (Source Zero) (Source Zero) = Source Zero
op2PruneZeros Add (Source Zero) y = pruneZeros y
op2PruneZeros Add x (Source Zero) = pruneZeros x
op2PruneZeros Add x y = Op2 Add (pruneZeros x) (pruneZeros y)

op2PruneZeros Sub (Source Zero) (Source Zero) = Source Zero
op2PruneZeros Sub (Source Zero) y = pruneZeros $ Elemwise Negate y
op2PruneZeros Sub x (Source Zero) = pruneZeros $ x
op2PruneZeros Sub x y = Op2 Sub (pruneZeros x) (pruneZeros y)
