-- Graph.hs

{-# OPTIONS_GHC -Wall #-}

module Numeric.Dvda.Graph( exprToGraph
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

import Numeric.Dvda.Expr
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
               | GUnary UnaryType Dim
               | GBinary BinaryType Dim
               | GOutput String Dim

instance (Eq a, Show a) => Show (GraphOp a) where
  show (GSource st _) = show st
  show (GUnary ewt _) = show ewt
  show (GBinary binaryt _) = show binaryt
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
  | otherwise           = case expr of (Source {})        -> newGraph
                                       ew@(Unary {})      -> graphGobbler newIdx (arg ew) newGraph
                                       binary@(Binary {}) -> graphGobbler newIdx (arg1 binary) $
                                                             graphGobbler newIdx (arg2 binary) newGraph
      where
        existingNode = lookupBy (\(_,_,a) -> a) expr (labEdges oldGraph)
        (existingIdx,_,_) = fromJust existingNode
        newIdx = head $ newNodes 1 oldGraph
        newGraph = ([], newIdx, getGraphOp expr, [(expr, parentIdx)]) & oldGraph

lookupBy :: Eq a => (b -> a) -> a -> [b] -> Maybe b
lookupBy f a bs = lookup a $ map (\x -> (f x, x)) bs

getGraphOp :: Expr a -> GraphOp a
getGraphOp src@(Source {}) = GSource (sourceType src) (dim src)
getGraphOp ew@(Unary {}) = GUnary (unaryType ew) (dim ew)
getGraphOp binary@(Binary {}) = GBinary (binaryType binary) (dim binary)
