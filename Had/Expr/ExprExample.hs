-- ExprExample.hs

{-# OPTIONS_GHC -Wall #-}

module Had.Expr.ExprExample( exprExample
                           , pruneExample
                           ) where

import Had.Expr.Expr
import Had.Expr.ExprToGraph
import Had.Simplify

exprExample :: IO ()
exprExample = do
  let exampleExpr :: Expr Integer
      exampleExpr = abs(x*34) + 5 + x
        where
          x = sym "x"
  
  print exampleExpr
  previewGraph $ exprToGraph exampleExpr


pruneExample :: IO ()
pruneExample = do
  let exampleExpr :: Expr Integer
      exampleExpr = y*(3*0*0+0*6 + 3) + 0*3
        where
          y = sym "y"
  print exampleExpr
  previewGraph $ exprToGraph exampleExpr
  previewGraph $ exprToGraph $ pruneZerosOnce exampleExpr
