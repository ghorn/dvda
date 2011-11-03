-- ExprToGraph.hs

{-# OPTIONS_GHC -Wall #-}

module Had.Expr.ExprToGraph( exprToGraph
                           , exprToGraphTest
                           , previewGraph
                           ) where

import Data.Graph.Inductive hiding (pre, nodes, edges)
import Data.GraphViz
import Data.Text.Lazy(pack)
import Data.Maybe

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
exprToGraph expr = exprsToGraph [expr]

exprsToGraph :: (Show a, Num a) => [Expr a] -> Gr (GraphOp a) (Expr a)
exprsToGraph exprs = foldr addOutput emptyGP labeledExprs
  where
    labeledExprs = reverse $ zipWith (\k e -> ("out"++show k, e)) [(0::Integer)..] exprs
    emptyGP = mkGraph [] []

addOutput :: (Show a, Num a) => (String, Expr a) -> Gr (GraphOp a) (Expr a) -> Gr (GraphOp a) (Expr a)
addOutput (name, expr) graph = graphGobbler newIdx expr newState
  where
    newIdx = noNodes graph
    newState = insNode (newIdx, GOutput name) graph

graphGobbler :: (Show a, Num a) => Int -> Expr a -> Gr (GraphOp a) (Expr a) -> Gr (GraphOp a) (Expr a)
graphGobbler parentIdx expr oldGraph = ret
  where
    existingNode = lookupBy (\(_,_,a) -> a) expr (labEdges oldGraph)
    newIdx = noNodes oldGraph
    newGraph
      | isNothing existingNode = ([], newIdx, getGraphOp expr, [(expr, parentIdx)]) & oldGraph
      | otherwise = insEdge (idx, parentIdx, expr) oldGraph
      where
        (idx,_,_) = (fromJust existingNode)
  
    ret = case expr of (Source _)     -> newGraph
                       (Elemwise _ x) -> graphGobbler newIdx x newGraph
                       (Op2 _ x y)    -> graphGobbler newIdx y $ graphGobbler newIdx x newGraph

lookupBy :: (Eq a) => (b -> a) -> a -> [b] -> Maybe b
lookupBy f a bs = lookup a $ map (\x -> (f x, x)) bs

getGraphOp :: (Show a, Eq a) => Expr a -> GraphOp a
getGraphOp (Source s) = GSource s
getGraphOp (Elemwise ewt _) = GElemwise ewt
getGraphOp (Op2 op2 _ _) = GOp2 op2

exprToGraphTest :: IO ()
exprToGraphTest = test

test :: IO ()
test = do
  let exampleExprs :: [Expr Integer]
      exampleExprs = [x*34 + 5 + x, 3*x + 3]
        where
          x = sym "x"
      g = exprsToGraph exampleExprs
  print exampleExprs
  print g
  previewGraph g
