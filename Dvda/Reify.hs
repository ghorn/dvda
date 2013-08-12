{-# OPTIONS_GHC -Wall #-}
{-# Language RankNTypes #-}
{-# Language BangPatterns #-}
{-# Language TypeFamilies #-}

-- This file is a modified version from Andy Gill's data-reify package
-- It is modified to use Data.HashTable.IO, which gives a speed improvement
-- at the expense of portability.

module Dvda.Reify ( MuRef(..)
                  , ReifyGraph(..)
                  , reifyGraphs
                  ) where

import Control.Concurrent.MVar ( newMVar, takeMVar, putMVar, MVar, readMVar )
import Control.Applicative ( Applicative )
import Data.Hashable ( Hashable(..) )
import Data.Traversable ( Traversable )
import qualified Data.Traversable as T
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
reifyGraphs :: (MuRef s, Traversable t) => t s -> IO (ReifyGraph (DeRef s), t Int)
reifyGraphs m = do
  stableNameMap <- H.new >>= newMVar
  graph <- newMVar []
  uVar <- newMVar 0

  let newUnique = do
        v <- takeMVar uVar
        let v' = succ v
        putMVar uVar v'
        return v'
  
  roots <- T.mapM (findNodes stableNameMap graph newUnique) m
  pairs <- readMVar graph
  return (ReifyGraph pairs, roots)

findNodes :: MuRef s
          => MVar (HashTable DynStableName Int)
          -> MVar [(Int,DeRef s Int)]
          -> IO Int
          -> s
          -> IO Int
findNodes stableNameMap graph newUnique !j = do
  st <- makeDynStableName j
  tab <- takeMVar stableNameMap
  amIHere <- H.lookup tab st
  case amIHere of
    -- if the j's StableName is already in the table, return the element
    Just var -> do putMVar stableNameMap tab
                   return var
    -- if j's StableName is not yet in the table, recursively call findNodes
    Nothing -> do var <- newUnique
                  H.insert tab st var
                  putMVar stableNameMap tab
                  res <- mapDeRef (findNodes stableNameMap graph newUnique) j
                  tab' <- takeMVar graph
                  putMVar graph $ (var,res) : tab'
                  return var

-- Stable names that not use phantom types.
-- As suggested by Ganesh Sittampalam.
newtype DynStableName = DynStableName (StableName ())

instance Hashable DynStableName where
  hashWithSalt salt (DynStableName sn) = hashWithSalt salt $ hashStableName sn
  
instance Eq DynStableName where
  (DynStableName sn1) == (DynStableName sn2) = sn1 == sn2

makeDynStableName :: a -> IO DynStableName
makeDynStableName a = do
  st <- makeStableName a
  return $ DynStableName (unsafeCoerce st)
