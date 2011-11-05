-- ExprToGraph.hs

{-# OPTIONS_GHC -Wall #-}

module Had.Expr.ExprToGraph( exprToGraph
                           , exprsToGraph
                           , exprToGraphTest
                           , previewGraph
                           , GraphOp(..)
                           , nodeToExpr
                           ) where

import Data.Graph.Inductive hiding (nodes, edges)
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
exprsToGraph exprs = foldr addOutput empty labeledExprs
  where
    labeledExprs = reverse $ zipWith (\k e -> ("out"++show k, e)) [(0::Integer)..] exprs

addOutput :: (Show a, Num a) => (String, Expr a) -> Gr (GraphOp a) (Expr a) -> Gr (GraphOp a) (Expr a)
addOutput (name, expr) graph = graphGobbler newIdx expr newState
  where
    newIdx = head $ newNodes 1 graph
    newState = insNode (newIdx, GOutput name) graph

nodeToExpr :: (Show a, Eq a) => Node -> Gr (GraphOp a) (Expr a) -> Expr a
nodeToExpr idx graph = f $ fromJust $ lab graph idx
  where
    children = pre graph idx
    f (GOutput _) = nodeToExpr (head children) graph
    f (GSource sourcetype) = Source sourcetype
    f (GElemwise elemwiseType) = Elemwise elemwiseType (nodeToExpr (head children) graph)
    f (GOp2 op2Type) = Op2 op2Type (nodeToExpr (children !! 0) graph) (nodeToExpr (children !! 1) graph)
        
graphGobbler :: (Show a, Num a) => Int -> Expr a -> Gr (GraphOp a) (Expr a) -> Gr (GraphOp a) (Expr a)
graphGobbler parentIdx expr oldGraph    
  | isJust existingNode = insEdge (existingIdx, parentIdx, expr) oldGraph
  | otherwise           = case expr of (Source _)     -> newGraph
                                       (Elemwise _ x) -> graphGobbler newIdx x newGraph
                                       (Op2 _ x y)    -> graphGobbler newIdx y $
                                                         graphGobbler newIdx x newGraph
      where
        existingNode = lookupBy (\(_,_,a) -> a) expr (labEdges oldGraph)
        (existingIdx,_,_) = fromJust existingNode
        newIdx = head $ newNodes 1 oldGraph
        newGraph = ([], newIdx, getGraphOp expr, [(expr, parentIdx)]) & oldGraph

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
      exampleExprs = [x*34 + 5 + x, x*34 + 3]
        where
          x = sym "x"
      g = exprsToGraph exampleExprs
  print exampleExprs
  print g
  previewGraph g
