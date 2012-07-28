{-# OPTIONS_GHC -Wall #-}
{-# Language FlexibleContexts #-}
{-# Language UndecidableInstances #-}

-- this file is a modified version from Andy Gill's data-reify package

module MutableDvda.ReifyGraph ( ReifyGraph(..)
                              ) where

data ReifyGraph e = ReifyGraph [(Unique,e Unique)]

type Unique = Int

-- | If 'e' is s Functor, and 'e' is 'Show'-able, then we can 'Show' a 'Graph'.
instance (Show (e Int)) => Show (ReifyGraph e) where
  show (ReifyGraph netlist) = show [ (u,e) | (u,e) <- netlist]
