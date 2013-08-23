{-# OPTIONS_GHC -Wall #-}

module Dvda.Algorithm.FunGraph
       ( FunGraph(..)
       , Node(..)
       , toFunGraph
       ) where

import Control.Applicative ( (<$>) )
import Data.Foldable ( Foldable )
import qualified Data.Foldable as F
import qualified Data.Graph as Graph
import qualified Data.HashSet as HS
import Data.Traversable ( Traversable )

import Dvda.Expr
import Dvda.Algorithm.Reify ( ReifyGraph(..), Node(..), reifyGraph )

data FunGraph f g a = FunGraph { fgInputs :: f Sym
                               , fgOutputs :: g Node
                               , fgReified :: [(Node, GExpr a Node)]
                               , fgTopSort :: [(Node, GExpr a Node)]
                               }

-- | find any symbols which are parents of outputs, but are not supplied by the user
detectMissingInputs :: Foldable f => f (Expr a) -> [(Node, GExpr a Node)] -> [Sym]
detectMissingInputs exprs gr = HS.toList $ HS.difference allGraphInputs allUserInputs
  where
    allUserInputs =
      let f (ESym name) acc = name : acc
          f _ _ = error $ "detectMissingInputs given non-ESym input" -- \"" ++ show e ++ "\""
      in HS.fromList $ F.foldr f [] exprs

    allGraphInputs =
      let f (_, GSym name) acc = name : acc
          f _ acc = acc
      in HS.fromList $ foldr f [] gr

-- | if the same input symbol (like ESym "x") is given at two different places throw an exception
findConflictingInputs :: Foldable f => f Sym -> [Sym]
findConflictingInputs syms = HS.toList redundant
  where
    redundant = snd $ F.foldl f (HS.empty, HS.empty) syms
      where
        f (knownExprs, redundantExprs) s
          | HS.member s knownExprs = (knownExprs, HS.insert s redundantExprs)
          | otherwise = (HS.insert s knownExprs, redundantExprs)


-- | Take inputs and outputs and traverse the outputs reifying all expressions
--   and creating a hashmap of StableNames. Once the hashmap is created,
--   lookup the provided inputs and return a FunGraph which contains an
--   expression graph, input/output indices, and other useful functions.
--   StableNames may be non-deterministic so this function may return graphs
--   with greater or fewer CSE's eliminated.
--   If CSE is then performed on the graph, the result is deterministic.
toFunGraph :: (Functor f, Foldable f, Traversable g) =>
              f (Expr a) -> g (Expr a) -> IO (FunGraph f g a)
toFunGraph inputExprs outputExprs = do
  -- reify the outputs
  (ReifyGraph rgr, outputIndices) <- reifyGraph outputExprs
  let userInputSyms = fmap f inputExprs
        where
          f (ESym s) = s
          f _ = error $ "ERROR: toFunGraph given non-ESym input" -- \"" ++ show x ++ "\""

      fg = FunGraph { fgInputs = userInputSyms
                    , fgOutputs = outputIndices
                    , fgReified = reverse rgr
                    , fgTopSort = topSort
                    }

      -- make sure all the inputs are symbolic, and find their indices in the Expr graph
      (gr, lookupVertex, lookupKey) =
        Graph.graphFromEdges $ map (\(k,gexpr) -> (gexpr, k, F.toList gexpr)) rgr
      lookupG k = (\(g,_,_) -> g) <$> lookupVertex <$> lookupKey k

      topSort = map lookup' $ reverse $ map ((\(_,k,_) -> k) . lookupVertex) $ Graph.topSort gr
        where
          lookup' k = case lookupG k of
            Nothing -> error "DVDA internal error"
            (Just g) -> (k,g)

  return $ case (detectMissingInputs inputExprs rgr, findConflictingInputs userInputSyms) of
    ([],[]) -> fg
    (xs,[]) -> error $ "toFunGraph found inputs that were not provided by the user: " ++ show xs
    ( _,xs) -> error $ "toFunGraph found conflicting inputs: " ++ show xs
