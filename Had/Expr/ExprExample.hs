-- ExprExample.hs

{-# OPTIONS_GHC -Wall #-}

module Had.Expr.ExprExample( exprExample 
                           ) where

import Data.GraphViz

import Had.Expr.Expr
import Had.Expr.ExprToGraph

exprExample :: IO ()
exprExample = do
  let exampleExpr :: Expr Integer
      exampleExpr = abs(x*34) + 5 + x
        where
          x = sym "x"
  
  print exampleExpr
  preview $ exprToGr exampleExpr
