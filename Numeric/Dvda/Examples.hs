-- Examples.hs

{-# OPTIONS_GHC -Wall #-}

module Numeric.Dvda.Examples( fadExample
                            , radExample
                            , exprExample
                            , pruneExample
                            ) where

import Numeric.Dvda.AD.Fad(fad)
import Numeric.Dvda.AD.Rad(rad)
import Numeric.Dvda.Expr.Expr(sym, Expr)
import Numeric.Dvda.Simplify(fastSimplify, pruneZerosOnce)
import Numeric.Dvda.Expr.ExprToGraph(previewGraph, exprToGraph, exprsToGraph)


fadExample :: IO ()
fadExample = do
  let f x = [x*34 + 5, x*34 + 4/x, sin x]
      y = sym "y" :: Expr Double
      expr = f y
      g = map fastSimplify $ fad f y

  print expr
  print $ expr
  print $ g
  previewGraph $ exprsToGraph expr
  previewGraph $ exprsToGraph g


  
radExample :: IO ()
radExample = do
  let exampleExpr :: Expr Double
      --      exampleExpr = abs(y*34) + 5 + x*y
      exampleExpr = z + x*y
        where
          x = sym "x"
          y = sym "y"
          z = sym "z"
  let args = map sym ["x", "y", "z"]

  print exampleExpr
  print $ rad exampleExpr args
  
  previewGraph $ exprToGraph exampleExpr
  previewGraph $ exprsToGraph (exampleExpr:(rad exampleExpr args))


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
