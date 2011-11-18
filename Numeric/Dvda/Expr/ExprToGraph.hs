-- ExprToGraph.hs

{-# OPTIONS_GHC -Wall #-}

module Numeric.Dvda.Expr.ExprToGraph( exprToGraph
                                    , exprsToGraph
                                    , lexprToGraph
                                    , lexprsToGraph
                                    , previewGraph
                                    , previewGraph_
                                    , GraphOp(..)
                                    , nodeToExpr
                                    ) where

import Data.Graph.Inductive hiding (nodes, edges)
import Data.GraphViz
import Data.Text.Lazy(pack)
import Data.Maybe
import Control.Concurrent(threadDelay)

import Numeric.Dvda.Expr.Expr
import Numeric.Dvda.Expr.SourceType
import Numeric.Dvda.Expr.ElemwiseType
import Numeric.Dvda.Expr.Op2Type
import Numeric.Dvda.Expr.Misc(outputNames)

previewGraph :: (DynGraph gr, Labellable nl, Show b) => gr nl b -> IO ()
previewGraph g = do
  preview $ emap show g
  threadDelay 10000

previewGraph_ :: (DynGraph gr, Labellable nl) => gr nl b -> IO ()
previewGraph_ g = do
  preview $ emap (\_ -> "") g
  threadDelay 10000

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

exprToGraph :: Eq a => Expr a -> Gr (GraphOp a) (Expr a)
exprToGraph expr = exprsToGraph [expr]

lexprToGraph :: Eq a => (String, Expr a) -> Gr (GraphOp a) (Expr a)
lexprToGraph labeledExpr = lexprsToGraph [labeledExpr]

exprsToGraph :: Eq a => [Expr a] -> Gr (GraphOp a) (Expr a)
exprsToGraph exprs = foldr addOutput empty labeledExprs
  where
    labeledExprs = reverse $ zip outputNames exprs

lexprsToGraph :: Eq a => [(String, Expr a)] -> Gr (GraphOp a) (Expr a)
lexprsToGraph labeledExprs = foldr addOutput empty labeledExprs

addOutput :: Eq a => (String, Expr a) -> Gr (GraphOp a) (Expr a) -> Gr (GraphOp a) (Expr a)
addOutput (name, expr) graph = graphGobbler newIdx expr newGraph
  where
    newIdx = head $ newNodes 1 graph
    newGraph = insNode (newIdx, GOutput name) graph

nodeToExpr :: Node -> Gr (GraphOp a) (Expr a) -> Expr a
nodeToExpr idx graph = f $ fromJust $ lab graph idx
  where
    children = pre graph idx
    f (GOutput _) = nodeToExpr (head children) graph
    f (GSource sourcetype) = Source sourcetype
    f (GElemwise elemwiseType) = Elemwise elemwiseType (nodeToExpr (head children) graph)
    f (GOp2 op2Type) = Op2 op2Type (nodeToExpr (children !! 0) graph) (nodeToExpr (children !! 1) graph)
        
graphGobbler :: Eq a => Int -> Expr a -> Gr (GraphOp a) (Expr a) -> Gr (GraphOp a) (Expr a)
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

lookupBy :: Eq a => (b -> a) -> a -> [b] -> Maybe b
lookupBy f a bs = lookup a $ map (\x -> (f x, x)) bs

getGraphOp :: Expr a -> GraphOp a
getGraphOp (Source s) = GSource s
getGraphOp (Elemwise ewt _) = GElemwise ewt
getGraphOp (Op2 op2 _ _) = GOp2 op2
