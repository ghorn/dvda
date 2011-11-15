-- ExprExample.hs

{-# OPTIONS_GHC -Wall #-}

module Numeric.Dvda.Expr.ExprExample( exprExample
                                    , pruneExample
                                    ) where

import Numeric.Dvda.Expr.Expr
import Numeric.Dvda.Expr.ExprToGraph
import Numeric.Dvda.Simplify

exprExample :: IO ()
exprExample = do
  let exampleExpr :: Expr Integer
      exampleExpr = abs(x*34) + 5 + x
        where
          x = sym "x"
  
  print exampleExpr
  previewGraph $ exprToGraph exampleExpr
  previewGraph $ exprToGraph $ pruneZerosOnce exampleExpr


pruneExample :: IO ()
pruneExample = do
  let exampleExpr :: Expr Integer
      exampleExpr = y*(3*0*0+0*6 + 3) + 0*3
        where
          y = sym "y"
  print exampleExpr
  previewGraph $ exprToGraph exampleExpr
  previewGraph $ exprToGraph $ pruneZerosOnce exampleExpr
