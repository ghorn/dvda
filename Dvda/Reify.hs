{-# OPTIONS_GHC -Wall #-}
{-# Language RankNTypes #-}
{-# Language TemplateHaskell #-}
{-# Language TypeFamilies #-}

-- this file is a modified version from Andy Gill's data-reify package

module Dvda.Reify ( MuRef(..)
                  , ReifyGraph(..)
                  , reifyGraphs
                  ) where

import Control.Concurrent.MVar ( newMVar, takeMVar, putMVar, MVar, readMVar )
import Control.Applicative ( Applicative )
import Data.Hashable ( Hashable, hash )
import System.Mem.StableName ( StableName, makeStableName, hashStableName )
import Unsafe.Coerce ( unsafeCoerce )

import Dvda.ReifyGraph ( ReifyGraph(..) )

import qualified Data.HashTable.IO as H
type HashTable k v = H.CuckooHashTable k v

class MuRef a where
  type DeRef a :: * -> *
  mapDeRef :: Applicative f
              => (forall b . (MuRef b, DeRef a ~ DeRef b) => b -> f u)
              -> a
              -> f (DeRef a u)

-- | 'reifyGraph' takes a data structure that admits 'MuRef', and returns a 'ReifyGraph' that contains
-- the dereferenced nodes, with their children as 'Int' rather than recursive values.

reifyGraphs :: MuRef s => [[[s]]] -> IO (ReifyGraph (DeRef s), [[[Int]]])
reifyGraphs m = do
  stableNameMap <- H.new >>= newMVar
  graph <- newMVar []
  uVar <- newMVar 0
  roots <- mapM (mapM (mapM (findNodes stableNameMap graph uVar))) m
  pairs <- readMVar graph
  return (ReifyGraph pairs, roots)

findNodes :: MuRef s
          => MVar (HashTable DynStableName Int)
          -> MVar [(Int,DeRef s Int)]
          -> MVar Int
          -> s
          -> IO Int
findNodes stableNameMap graph uVar j | j `seq` True = do
  st <- makeDynStableName j
  tab <- takeMVar stableNameMap
  amIHere <- H.lookup tab st
  case amIHere of
    -- if the j's StableName is already in the table, return the element
    Just var -> do putMVar stableNameMap tab
                   return var
    -- if j's StableName is not yet in the table, recursively call findNodes
    Nothing -> do var <- newUnique uVar
                  H.insert tab st var
                  putMVar stableNameMap tab
                  res <- mapDeRef (findNodes stableNameMap graph uVar) j
                  tab' <- takeMVar graph
                  putMVar graph $ (var,res) : tab'
                  return var
findNodes _ _ _ _ = error "findNodes: strictness seq function failed to return True"

newUnique :: MVar Int -> IO Int
newUnique var = do
  v <- takeMVar var
  let v' = succ v
  putMVar var v'
  return v'
  
-- Stable names that not use phantom types.
-- As suggested by Ganesh Sittampalam.
data DynStableName = DynStableName (StableName ())

instance Hashable DynStableName where
  hash (DynStableName sn) = hashStableName sn
  
instance Eq DynStableName where
	(DynStableName sn1) == (DynStableName sn2) = sn1 == sn2

makeDynStableName :: a -> IO DynStableName
makeDynStableName a = do
	st <- makeStableName a
	return $ DynStableName (unsafeCoerce st)
