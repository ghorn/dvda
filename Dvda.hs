{- |
   Module      : Dvda
   Description : Top level module

   This is the top level module which exports the API
 -}

{-# OPTIONS_GHC -Wall #-}

module Dvda ( -- * primitives
              sym
            , vsym
            , msym
            , vec
            , mat
              -- * operations
            , scale
            , dot
            , diff
              -- * symbolic expression type
            , Expr
              -- * construct FunGraphs
            , FunGraph
            , makeFunGraph
            , runFunGraph
            , inputs_
            , outputs_
            , node
              -- * show/summarize FunGraphs
            , funGraphSummary
            , funGraphSummary'
            , showCollisions
            , previewGraph
              -- * compile and link function
            , buildHSFunction
              -- * Heterogenous inputs/outputs
            , (:*)(..)
            , Exprs
            ) where

import Dvda.Expr
import Dvda.Graph
import Dvda.HSBuilder
import Dvda.SymMonad


