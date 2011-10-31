-- Rad.hs

{-# OPTIONS_GHC -Wall #-}

module Had.Rad( diffR
              , radExample
              ) where

import Had.Expr
import Had.Expr.Op2Type(op2DiffRule)
import Had.Expr.ElemwiseType(elemwiseDiffRule)
import Had.Simplify(pruneZeros)

import Data.GraphViz

diffR :: (Show a, Eq a, Num a) => Expr a -> Expr a
diffR (Source (Sym name)) = Source $ Sym ("d(" ++ name ++ ")")
diffR (Source _) = Source Zero
diffR (Op2 op2Type x y) = pruneZeros $ op2DiffRule op2Type (x, diffR x) (y, diffR y)
diffR (Elemwise elemwiseType x) = pruneZeros $ elemwiseDiffRule elemwiseType (x, diffR x)

radExample :: IO ()
radExample = do
  let exampleExpr :: Expr Integer
      exampleExpr = abs(y*34) + 5 + x*y
        where
          x = sym "x"
          y = sym "y"
  print exampleExpr
  print $ diffR exampleExpr
  preview $ exprToGraph exampleExpr
  preview $ exprToGraph $ diffR exampleExpr
