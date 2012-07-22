{-# OPTIONS_GHC -Wall #-}
{-# Language GADTs #-}

module MutableDvda.SharedVar ( SVMonad
                             , SharedVar
                             , newSharedVar
                             , readSharedVar
                             , toIO
                             , unsafePerformSV
                             ) where

import System.IO.Unsafe ( unsafePerformIO )

---------------------------- use MVars ----------------------
import Control.Concurrent.MVar

type SVMonad = IO
type SharedVar = MVar

newSharedVar :: a -> SVMonad (SharedVar a)
newSharedVar = newMVar

readSharedVar :: SharedVar a -> SVMonad a
readSharedVar = readMVar

toIO :: SVMonad a -> IO a
toIO = id

------------------------ use TMVars ----------------------------
--import Control.Concurrent.STM ( STM, atomically )
--import Control.Concurrent.STM.TMVar ( TMVar, newTMVar, readTMVar )
--
--type SVMonad = STM
--type SharedVar = TMVar
--
--newSharedVar :: a -> SVMonad (SharedVar a)
--newSharedVar = newTMVar
--
--readSharedVar :: SharedVar a -> SVMonad a
--readSharedVar = readTMVar
--
--toIO :: SVMonad a -> IO a
--toIO = atomically

------------------------ use TVars ----------------------------
--import Control.Concurrent.STM ( STM, atomically )
--import Control.Concurrent.STM.TVar ( TVar, newTVar, readTVar )
--
--type SVMonad = STM
--type SharedVar = TVar
--
--newSharedVar :: a -> SVMonad (SharedVar a)
--newSharedVar = newTVar
--
--readSharedVar :: SharedVar a -> SVMonad a
--readSharedVar = readTVar
--
--toIO :: SVMonad a -> IO a
--toIO = atomically

unsafePerformSV :: SVMonad a -> a
unsafePerformSV = unsafePerformIO . toIO
