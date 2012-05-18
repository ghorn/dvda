{- |
   Module      : Dvda
   Description : Top level module

   This is the top level module which exports the API
 -}

{-# OPTIONS_GHC -Wall #-}

module Dvda ( -- * Core API
              module Dvda.SymMonad
              -- * Tensor Expression
            , module Dvda.Expr
              -- * Differentiation rules
            , module Dvda.Dual
            ) where

import Dvda.SymMonad
import Dvda.Expr
import Dvda.Dual
