{-# OPTIONS_GHC -Wall #-}
{-# Language TypeOperators #-}
{-# Language TypeFamilies #-}
{-# Language FlexibleInstances #-}

module Dvda.FunGraph ( FunGraph
                     , ToFunGraph
                     , NumT
                     , (:*)(..)
                     , MVS(..)
                     , toFunGraph
                     , countNodes
                     , fgInputs
                     , fgOutputs
                     , fgLookupGExpr
                     , fgReified
                     , topSort
--                     , fgGraph
                     , nodelistToFunGraph
                     ) where

import Control.Applicative
import Data.Foldable ( Foldable )
import qualified Data.Foldable as F
import qualified Data.Graph as Graph
import Data.Hashable ( Hashable )
import qualified Data.HashSet as HS
import Data.Traversable ( Traversable )
import qualified Data.Traversable as T

import Dvda.Expr
import Dvda.Reify ( ReifyGraph(..), reifyGraphs )

data FunGraph a = FunGraph { fgGraph :: Graph.Graph
                           , fgInputs :: [MVS (GExpr a Int)]
                           , fgOutputs :: [MVS Int]
                           , fgReified :: [(Int, GExpr a Int)]
                           , fgLookupGExpr :: (Int -> Maybe (GExpr a Int))
                           , fgVertexFromKey :: Int -> Maybe Int
                           , fgNodeFromVertex :: Int -> (GExpr a Int, Int, [Int])
                           }

instance Show a => Show (FunGraph a) where
  show fg = "FunGraph\ninputs:\n" ++ show (fgInputs fg) ++ "\noutputs:\n" ++ show (fgOutputs fg) ++ "\ngraph:\n" ++ show (fgGraph fg)

---- | matrix or vector or scalar
data MVS a = Mat [[a]] | Vec [a] | Sca a deriving Show

instance Functor MVS where
  fmap f (Sca x)  = Sca (f x)
  fmap f (Vec xs) = Vec (map f xs)
  fmap f (Mat xs) = Mat (map (map f) xs)

instance Foldable MVS where
  foldr f x0 (Sca x)  = foldr f x0 [x]
  foldr f x0 (Vec xs) = foldr f x0 xs
  foldr f x0 (Mat xs) = foldr f x0 (concat xs)

instance Traversable MVS where
  traverse f (Sca x)  = Sca <$> f x
  traverse f (Vec xs) = Vec <$> T.traverse f xs
  traverse f (Mat xs) = Mat <$> T.traverse (T.traverse f) xs

class ToFunGraph a where
  type NumT a
  toMVSList :: a -> [MVS (Expr (NumT a))]
instance ToFunGraph (Expr a) where
  type NumT (Expr a) = a
  toMVSList x = [Sca x]
instance ToFunGraph [Expr a] where
  type NumT [Expr a] = NumT (Expr a)
  toMVSList x = [Vec x]
instance ToFunGraph [[Expr a]] where
  type NumT [[Expr a]] = NumT [Expr a]
  toMVSList x = [Mat x]

data a :* b = a :* b deriving Show
infixr 6 :*
instance (ToFunGraph a, ToFunGraph b, NumT a ~ NumT b) => ToFunGraph (a :* b) where
  type NumT (a :* b) = NumT a
  toMVSList (x :* y) = toMVSList x ++ toMVSList y

-- | find any symbols which are parents of outputs, but are not supplied by the user
detectMissingInputs :: (Eq a, Hashable a, Show a) => [MVS (Expr a)] -> [(Int,GExpr a Int)] -> [GExpr a Int]
detectMissingInputs exprs gr = HS.toList $ HS.difference allGraphInputs allUserInputs
  where
    allUserInputs = let f (ESym name) acc = (GSym name):acc
                        f _ e = error $ "detectMissingInputs given non-ESym input \"" ++ show e ++ "\""
                    in HS.fromList $ foldr f [] (concatMap F.toList exprs)

    allGraphInputs = let f (_,(GSym name)) acc = (GSym name):acc
                         f _ acc = acc
                     in HS.fromList $ foldr f [] gr

-- | if the same input symbol (like ESym "x") is given at two different places throw an exception
findConflictingInputs :: (Eq a, Hashable a, Show a) => [MVS (Expr a)] -> [Expr a]
findConflictingInputs exprs = HS.toList redundant
  where
    redundant = snd $ foldl f (HS.empty, HS.empty) (concatMap F.toList exprs)
      where
        f (knownExprs, redundantExprs) expr@(ESym _)
          | HS.member expr knownExprs = (knownExprs, HS.insert expr redundantExprs)
          | otherwise = (HS.insert expr knownExprs, redundantExprs)
        f _ e = error $ "findConflictingInputs saw non-ESym input \"" ++ show e ++ "\""


-- | Take inputs and outputs which are of classes ToFunGraph (heterogenous lists of @Expr a@)
--   and traverse the outputs reifying all expressions and creating a hashmap of StableNames (stable pointers).
--   Once the hashmap is created, lookup the provided inputs and return a FunGraph which contains an
--   expression graph, input/output indices, and other useful functions. StableNames is non-deterministic
--   so this function may return graphs with more or fewer CSE's eliminated.
--   If CSE is then performed on the graph, the result is deterministic.
toFunGraph :: (Eq a, Hashable a, Show a, ToFunGraph b, ToFunGraph c, NumT b ~ a, NumT c ~ a)
              => b -> c -> IO (FunGraph a)
toFunGraph inputs outputs = mvsToFunGraph (toMVSList inputs) (toMVSList outputs)

mvsToFunGraph :: (Eq a, Hashable a, Show a) => [MVS (Expr a)] -> [MVS (Expr a)] -> IO (FunGraph a)
mvsToFunGraph inputMVSExprs outputMVSExprs = do
  -- reify the outputs
  (ReifyGraph rgr, outputMVSIndices) <- reifyGraphs outputMVSExprs
  let fg = nodelistToFunGraph rgr inputMVSGExprs outputMVSIndices
      inputMVSGExprs = map (fmap f) inputMVSExprs
        where
          f (ESym name) = (GSym name)
          f x = error $ "ERROR: mvsToFunGraph given non-ESym input \"" ++ show x ++ "\""
  return $ case (detectMissingInputs inputMVSExprs rgr, findConflictingInputs inputMVSExprs) of
    ([],[]) -> fg
    (xs,[]) -> error $ "mvsToFunGraph found inputs that were not provided by the user: " ++ show xs
    ( _,xs) -> error $ "mvsToFunGraph found idential inputs set more than once: " ++ show xs

nodelistToFunGraph :: [(Int,GExpr a Int)] -> [MVS (GExpr a Int)] -> [MVS Int] -> FunGraph a
nodelistToFunGraph rgr inputMVSIndices outputMVSIndices =
  FunGraph { fgGraph = gr
           , fgInputs = inputMVSIndices
           , fgOutputs = outputMVSIndices
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
countNodes :: FunGraph a -> Int
countNodes = length . Graph.vertices . fgGraph

topSort :: FunGraph a -> [Int]
topSort fg = map ((\(_,k,_) -> k) . (fgNodeFromVertex fg)) $ Graph.topSort (fgGraph fg)
