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

import Control.Concurrent.MVar ( newMVar, takeMVar, putMVar, MVar, readMVar, modifyMVar_ )
import Control.Applicative ( Applicative )
import Data.Hashable ( Hashable(..) )
import Data.Traversable ( Traversable )
import qualified Data.Traversable as T
import System.Mem.StableName ( StableName, makeStableName, hashStableName )
import Unsafe.Coerce ( unsafeCoerce )

import qualified Data.HashTable.IO as H
type HashTable k v = H.CuckooHashTable k v

type Unique = Int

data ReifyGraph e = ReifyGraph [(Unique,e Unique)]

class MuRef a where
  type DeRef a :: * -> *
  mapDeRef :: Applicative f
              => (forall b . (MuRef b, DeRef a ~ DeRef b) => b -> f u)
              -> a
              -> f (DeRef a u)

-- | 'reifyGraph' takes a data structure that admits 'MuRef', and returns a 'ReifyGraph' that contains
-- the dereferenced nodes, with their children as 'Int' rather than recursive values.
reifyGraphs :: (MuRef s, Traversable t) => t s -> IO (ReifyGraph (DeRef s), t Unique)
reifyGraphs m = do
  stableNameMap <- H.new :: IO (HashTable DynStableName Unique)
  uVar <- newMVar 0

  let newUnique :: IO Int
      newUnique = do
        v <- takeMVar uVar
        putMVar uVar (succ v)
        return v

      findNodes :: MuRef s => MVar [(Unique,DeRef s Unique)] -> s -> IO Unique
      findNodes graph !j = do
        stableName <- makeDynStableName j
        amIHere <- H.lookup stableNameMap stableName
        case amIHere of
          -- if the j's StableName is already in the table, return the element
          Just unique -> return unique
          -- if j's StableName is not yet in the table, recursively call findNodes
          Nothing -> do
            unique <- newUnique
            H.insert stableNameMap stableName unique
            res <- mapDeRef (findNodes graph) j
            modifyMVar_ graph (return . ((unique,res):))
            return unique

  graph0 <- newMVar []
  roots <- T.mapM (findNodes graph0) m
  pairs <- readMVar graph0
  return (ReifyGraph pairs, roots)


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
