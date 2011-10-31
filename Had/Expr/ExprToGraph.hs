-- ExprToGraph.hs

{-# OPTIONS_GHC -Wall #-}

module Had.Expr.ExprToGraph( exprToGraph
                           , exprToGraphTest
                           , previewGraph
                           ) where

import Data.Graph.Inductive hiding (pre, nodes, edges)
import Control.Monad.State
import Data.GraphViz
import Data.Text.Lazy(pack)

import Had.Expr.Expr
import Had.Expr.SourceType
import Had.Expr.ElemwiseType
import Had.Expr.Op2Type

previewGraph :: (Show a, Eq a) => Gr (GraphOp a) (Expr a) -> IO ()
previewGraph g = preview $ emap show g
                
data GraphOp a = GSource (SourceType a)
               | GElemwise ElemwiseType
               | GOp2 Op2Type
               | GOutput String
instance (Eq a, Show a) => Show (GraphOp a) where
  show (GSource sourceType) = show sourceType
  show (GElemwise elemwiseType) = show elemwiseType
  show (GOp2 op2Type) = show op2Type
  show (GOutput name) = name


instance (Show a, Eq a) => Labellable (GraphOp a) where
  toLabelValue go = toLabelValue $ pack $ show go

exprToGraph :: (Show a, Num a) => Expr a -> Gr (GraphOp a) (Expr a)
exprToGraph expr = evalState (graphGobbler 0 expr) emptyGP
  where
    emptyGP = mkGraph [(0, GOutput "out")] []

getGraphOp :: (Show a, Eq a) => Expr a -> GraphOp a
getGraphOp (Source s) = GSource s
getGraphOp (Elemwise ewt _) = GElemwise ewt
getGraphOp (Op2 op2 _ _) = GOp2 op2


graphGobbler :: (Show a, Num a) => Int -> Expr a -> State (Gr (GraphOp a) (Expr a)) (Gr (GraphOp a) (Expr a))
graphGobbler parentIdx expr = do
  graph <- get
  let newIdx = noNodes graph
      newState = ([], newIdx, getGraphOp expr, [(expr, parentIdx)]) & graph
  
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
  previewGraph g
