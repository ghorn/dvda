{-# OPTIONS_GHC -Wall #-}
{-# Language TemplateHaskell #-}
{-# Language TypeOperators #-}
{-# Language TypeFamilies #-}
{-# Language FlexibleInstances #-}
-- {-# Language FlexibleContexts #-}

module MutableDvda.FunGraph ( FunGraph(..)
                            , ToFunGraph
                            , (:*)(..)
                            , toFunGraph
                            ) where

import FileLocation ( err )
import MutableDvda.Expr
import MutableDvda.Reify

data FunGraph a = FunGraph [(Int, GExpr a Int)] [MVS Int] [MVS Int]

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
  (ReifyGraph gr, [inputIndices, outputIndices]) <- reifyGraphs (map (map mvsToLists) [inputExprs, outputExprs])
  return $ FunGraph gr
    (zipWith listsToMVS inputExprs inputIndices)
    (zipWith listsToMVS outputExprs outputIndices)
