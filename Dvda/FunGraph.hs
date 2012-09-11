{-# OPTIONS_GHC -Wall #-}

module Dvda.FunGraph ( FunGraph
                     , toFunGraph
                     , countNodes
                     , fgInputs
                     , fgOutputs
                     , fgLookupGExpr
                     , fgReified
                     , topSort
--                     , fgGraph
                     , nodelistToFunGraph
                     , exprsToFunGraph
                     ) where

import Control.Applicative
import Data.Foldable ( Foldable )
import qualified Data.Foldable as F
import qualified Data.Graph as Graph
import Data.Hashable ( Hashable )
import qualified Data.HashSet as HS
import Data.Traversable ( Traversable )

import Dvda.Expr
import Dvda.Reify ( ReifyGraph(..), reifyGraphs )

data FunGraph a f g = FunGraph { fgGraph :: Graph.Graph
                               , fgInputs :: f (GExpr a Int)
                               , fgOutputs :: g Int
                               , fgReified :: [(Int, GExpr a Int)]
                               , fgLookupGExpr :: Int -> Maybe (GExpr a Int)
                               , fgVertexFromKey :: Int -> Maybe Int
                               , fgNodeFromVertex :: Int -> (GExpr a Int, Int, [Int])
                               }

--instance (Show a) => Show (FunGraph a f g) where
--  show fg = "FunGraph\ninputs:\n" ++ show (fgInputs fg) ++ "\noutputs:\n" ++ show (fgOutputs fg) ++ "\ngraph:\n" ++ show (fgGraph fg)

-- | find any symbols which are parents of outputs, but are not supplied by the user
detectMissingInputs :: (Foldable f, Eq a, Hashable a, Show a) => f (Expr a) -> [(Int,GExpr a Int)] -> [GExpr a Int]
detectMissingInputs exprs gr = HS.toList $ HS.difference allGraphInputs allUserInputs
  where
    allUserInputs = let f (ESym name) acc = (GSym name):acc
                        f _ e = error $ "detectMissingInputs given non-ESym input \"" ++ show e ++ "\""
                    in HS.fromList $ F.foldr f [] exprs

    allGraphInputs = let f (_,(GSym name)) acc = (GSym name):acc
                         f _ acc = acc
                     in HS.fromList $ foldr f [] gr

-- | if the same input symbol (like ESym "x") is given at two different places throw an exception
findConflictingInputs :: (Foldable f, Eq a, Hashable a, Show a) => f (Expr a) -> [Expr a]
findConflictingInputs exprs = HS.toList redundant
  where
    redundant = snd $ F.foldl f (HS.empty, HS.empty) exprs
      where
        f (knownExprs, redundantExprs) expr@(ESym _)
          | HS.member expr knownExprs = (knownExprs, HS.insert expr redundantExprs)
          | otherwise = (HS.insert expr knownExprs, redundantExprs)
        f _ e = error $ "findConflictingInputs saw non-ESym input \"" ++ show e ++ "\""


-- | Take inputs and outputs and traverse the outputs reifying all expressions
--   and creating a hashmap of StableNames (stable pointers).
--   Once the hashmap is created, lookup the provided inputs and return a FunGraph which contains an
--   expression graph, input/output indices, and other useful functions. StableNames may be non-deterministic
--   so this function may return graphs with more or fewer CSE's eliminated.
--   If CSE is then performed on the graph, the result is deterministic.
toFunGraph :: (Functor f, Foldable f, Traversable g, Eq a, Hashable a, Show a) =>
              f (Expr a) -> g (Expr a) -> IO (FunGraph a f g)
toFunGraph inputExprs outputExprs = do
  -- reify the outputs
  (ReifyGraph rgr, [outputIndices]) <- reifyGraphs [outputExprs]
  let fg = nodelistToFunGraph rgr inputGExprs outputIndices
      inputGExprs = fmap f inputExprs
        where
          f (ESym name) = (GSym name)
          f x = error $ "ERROR: toFunGraph given non-ESym input \"" ++ show x ++ "\""
  return $ case (detectMissingInputs inputExprs rgr, findConflictingInputs inputExprs) of
    ([],[]) -> fg
    (xs,[]) -> error $ "toFunGraph found inputs that were not provided by the user: " ++ show xs
    ( _,xs) -> error $ "toFunGraph found idential inputs set more than once: " ++ show xs

nodelistToFunGraph :: [(Int,GExpr a Int)] -> f (GExpr a Int) -> g Int -> FunGraph a f g
nodelistToFunGraph rgr inputIndices outputIndices =
  FunGraph { fgGraph = gr
           , fgInputs = inputIndices
           , fgOutputs = outputIndices
           , fgLookupGExpr = lookupG
           , fgReified = rgr
           , fgVertexFromKey = lookupKey
           , fgNodeFromVertex = lookupVertex
           }
  where
    -- make sure all the inputs are symbolic, and find their indices in the Expr graph
    (gr, lookupVertex, lookupKey) = Graph.graphFromEdges $ map (\(k,gexpr) -> (gexpr, k, getParents gexpr)) rgr
    lookupG k = (\(g,_,_) -> g) <$> lookupVertex <$> lookupKey k


---------------------------------- utilities -----------------------------
countNodes :: FunGraph a f g -> Int
countNodes = length . Graph.vertices . fgGraph

topSort :: FunGraph a f g -> [Int]
topSort fg = map ((\(_,k,_) -> k) . (fgNodeFromVertex fg)) $ Graph.topSort (fgGraph fg)

-- | make a FunGraph out of outputs, automatically detecting the proper inputs
exprsToFunGraph :: (Eq a, Show a, Hashable a, Traversable g) => g (Expr a) -> IO (FunGraph a [] g)
exprsToFunGraph outputs = do
  let getSyms :: [Expr a] -> [Sym]
      getSyms exprs = HS.toList $ foldr (\acc expr -> foldExpr f expr acc) HS.empty exprs
        where
          f (ESym s) hs = HS.insert s hs
          f _ hs = hs
      inputs = map ESym $ getSyms (F.toList outputs)
  toFunGraph inputs outputs
