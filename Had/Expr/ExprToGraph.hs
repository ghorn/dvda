-- ExprToGraph.hs

{-# OPTIONS_GHC -Wall #-}

module Had.Expr.ExprToGraph( exprToGraph
                           , exprToGraphTest
                           ) where

import Data.Graph.Inductive hiding (pre, nodes, edges)
import Control.Monad.State
import Data.GraphViz

import Had.Expr.Expr

exprToGraph :: (Show a, Num a) => Expr a -> (Gr String String)
exprToGraph expr = evalState (graphGobbler 0 expr) emptyGP
  where
    emptyGP :: Gr String String
    emptyGP = mkGraph [(0, "output")] []


graphGobbler :: (Show a, Num a) => Int -> Expr a -> State (Gr String String) (Gr String String)
graphGobbler parentIdx expr = do
  graph <- get
  let newIdx = noNodes graph
      newState = ([], newIdx, showExprOp expr, [(show expr, parentIdx)]) & graph
  put newState
  case expr of (Source _) -> do
                 return newState
               (Elemwise _ x) -> do
                 graphGobbler newIdx x
               (Op2 _ x y) -> do
                 _ <- graphGobbler newIdx x
                 graphGobbler newIdx y


exprToGraphTest :: IO ()
exprToGraphTest = test

test :: IO ()
test = do
  let exampleExpr :: Expr Integer
      exampleExpr = abs(x*34) + 5 + x
        where
          x = sym "x"
      g = exprToGraph exampleExpr
  print exampleExpr
  print g
  preview $ g
