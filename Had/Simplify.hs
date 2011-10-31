-- Simplify.hs

{-# OPTIONS_GHC -Wall #-}

module Had.Simplify( pruneZeros
                   , pruneExample
                   ) where

import Had.Expr

import Data.GraphViz

pruneZeros :: (Show a, Eq a, Num a) => Expr a -> Expr a
pruneZeros x 
  | x == xPruned = x
  | otherwise    = pruneZeros xPruned
  where
    xPruned = pruneZeros' x

pruneZeros' :: (Show a, Eq a, Num a) => Expr a -> Expr a
pruneZeros' (Op2 op2Type x y) = op2PruneZeros op2Type x y
pruneZeros' (Elemwise elemwiseType x) = elemwisePruneZeros elemwiseType x
pruneZeros' (Source x) = (Source x)

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

pruneExample :: IO ()
pruneExample = do
  let exampleExpr :: Expr Integer
      exampleExpr = y*(3*0*0+0*6 + 3) + 0*3
        where
          y = sym "y"
  print exampleExpr
  preview $ exprToGraph exampleExpr
  preview $ exprToGraph $ pruneZeros exampleExpr
