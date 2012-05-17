{- |
   Module      : Dvda
   Description : Top level module

   This is the top level module which exports the API
 -}

{-# OPTIONS_GHC -Wall #-}

module Dvda ( -- * Core API
              module Dvda.SymMonad
              -- * Differentiation rules
            , module Dvda.Dual
            ) where

import Dvda.SymMonad
import Dvda.Dual
