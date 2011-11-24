-- ExprToGraph.hs

{-# OPTIONS_GHC -Wall #-}

module Numeric.Dvda.Expr.ExprToGraph( exprToGraph
                                    , exprsToGraph
                                    , lexprToGraph
                                    , lexprsToGraph
                                    , previewGraph
                                    , previewGraph_
                                    , GraphOp(..)
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

data GraphOp a = GSource (SourceType a) Dim
               | GElemwise ElemwiseType Dim
               | GOp2 Op2Type Dim
               | GOutput String Dim

instance (Eq a, Show a) => Show (GraphOp a) where
  show (GSource st _) = show st
  show (GElemwise ewt _) = show ewt
  show (GOp2 op2t _) = show op2t
  show (GOutput name _) = name


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
    newGraph = insNode (newIdx, GOutput name (dim expr)) graph

graphGobbler :: Eq a => Int -> Expr a -> Gr (GraphOp a) (Expr a) -> Gr (GraphOp a) (Expr a)
graphGobbler parentIdx expr oldGraph    
  | isJust existingNode = insEdge (existingIdx, parentIdx, expr) oldGraph
  | otherwise           = case expr of (Source {})      -> newGraph
                                       ew@(Elemwise {}) -> graphGobbler newIdx (arg ew) newGraph
                                       op2@(Op2 {})     -> graphGobbler newIdx (arg2 op2) $
                                                           graphGobbler newIdx (arg1 op2) newGraph
      where
        existingNode = lookupBy (\(_,_,a) -> a) expr (labEdges oldGraph)
        (existingIdx,_,_) = fromJust existingNode
        newIdx = head $ newNodes 1 oldGraph
        newGraph = ([], newIdx, getGraphOp expr, [(expr, parentIdx)]) & oldGraph

lookupBy :: Eq a => (b -> a) -> a -> [b] -> Maybe b
lookupBy f a bs = lookup a $ map (\x -> (f x, x)) bs

getGraphOp :: Expr a -> GraphOp a
getGraphOp src@(Source {}) = GSource (sourceType src) (dim src)
getGraphOp ew@(Elemwise {}) = GElemwise (elemwiseType ew) (dim ew)
getGraphOp op2@(Op2 {}) = GOp2 (op2Type op2) (dim op2)
