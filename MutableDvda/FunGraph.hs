{-# OPTIONS_GHC -Wall #-}
{-# Language TemplateHaskell #-}
{-# Language TypeOperators #-}
{-# Language TypeFamilies #-}
{-# Language FlexibleInstances #-}
{-# Language FlexibleContexts #-}

module MutableDvda.FunGraph ( FunGraph
                            , ToFunGraph
                            , NumT
                            , (:*)(..)
                            , MVS(..)
                            , toFunGraph
                            , countNodes
                            , fgInputs
                            , fgOutputs
                            , lookupGExpr
                            , topSort
                            , getRedundantExprs
                            ) where

import Control.Applicative
import qualified Data.Graph as Graph
import Data.Hashable ( Hashable )
import Data.HashSet ( HashSet )
import qualified Data.HashSet as HS
import FileLocation ( err )
import MutableDvda.Expr
import MutableDvda.Reify

data FunGraph a = FunGraph Graph.Graph [MVS Int] [MVS Int] (Int -> Maybe (GExpr a Int))

---- | matrix or vector or scalar
data MVS a = Mat [[a]] | Vec [a] | Sca a deriving Show

mvsToLists :: MVS a -> [[a]]
mvsToLists (Mat x) = x
mvsToLists (Vec x) = [x]
mvsToLists (Sca x) = [[x]]

listsToMVS :: MVS a -> [[b]] -> MVS b
listsToMVS (Sca _) [[x]] = Sca x
listsToMVS (Sca _) _ = $(err "reifyGraphs returned non-scalar output for scalar input")
listsToMVS (Vec []) [] = Vec []
listsToMVS (Vec v) [x]
  | length v == length x = Vec x
listsToMVS (Vec _) _ = $(err "reifyGraphs returned different number of output indices than inputs for Vec")
listsToMVS (Mat m) x
  | length m == length x && and (zipWith (\u v -> length u == length v) m x) = Mat x
  | otherwise = $(err "reifyGraphs returned different number of output indices than inputs for Mat")

-- | throw an error if there are repeated inputs
getRedundantExprs :: (Eq (NumT a), Hashable (NumT a), ToFunGraph a)
                     => a -> Maybe (HashSet (Expr (NumT a)))
getRedundantExprs exprs
  | HS.null redundant = Nothing
  | otherwise = Just redundant
  where
    redundant = snd $ foldl f (HS.empty, HS.empty) (concat $ concatMap mvsToLists (toMVSList exprs))
      where
        f (knownExprs, redundantExprs) expr
          | HS.member expr knownExprs = (knownExprs, HS.insert expr redundantExprs)
          | otherwise = (HS.insert expr knownExprs, redundantExprs)


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

toFunGraph :: (ToFunGraph a, ToFunGraph b, NumT a ~ NumT b) => a -> b -> IO (FunGraph (NumT a))
toFunGraph inputs outputs = mvsToFunGraph (toMVSList inputs) (toMVSList outputs)

mvsToFunGraph :: [MVS (Expr a)] -> [MVS (Expr a)] -> IO (FunGraph a)
mvsToFunGraph inputExprs outputExprs = do
  (ReifyGraph rgr, [inputIndices, outputIndices]) <- reifyGraphs (map (map mvsToLists) [inputExprs, outputExprs])
  let inputMVSIndices  = zipWith listsToMVS  inputExprs  inputIndices
      outputMVSIndices = zipWith listsToMVS outputExprs outputIndices
      (gr, lookupVertex, lookupKey) = Graph.graphFromEdges $ map (\(k,gexpr) -> (gexpr, k, getParents gexpr)) rgr
      lookupG k = (\(g,_,_) -> g) <$> lookupVertex <$> lookupKey k
  return $ FunGraph gr inputMVSIndices outputMVSIndices lookupG


---------------------------------- utilities -----------------------------
countNodes :: FunGraph a -> Int
countNodes (FunGraph gr _ _ _) = length (Graph.vertices gr)

topSort :: FunGraph a -> [Int]
topSort (FunGraph gr _ _ _) = Graph.topSort gr

lookupGExpr :: Int -> FunGraph a -> Maybe (GExpr a Int)
lookupGExpr k (FunGraph _ _ _ lookupFun) = lookupFun k

fgInputs :: FunGraph a -> [MVS Int]
fgInputs (FunGraph _ ins _ _) = ins

fgOutputs :: FunGraph a -> [MVS Int]
fgOutputs (FunGraph _ _ outs _) = outs
