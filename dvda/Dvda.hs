{- |
   Module      : Dvda
   Description : Top level module

   This is the top level module which exports the API
 -}

{-# OPTIONS_GHC -Wall #-}

module Dvda ( -- * primitives
              sym
            , symDependent
            , symDependentN
              -- * operations
            , rad
              -- * symbolic expression type
            , Expr
              -- * construct FunGraphs
--            , cse
              -- * show/summarize FunGraphs
--            , previewGraph
--            , previewGraph'
              -- * compile and link function
--            , buildHSFunction
--            , buildHSFunctionPure
--            , buildHSFunctionFromGraph
            , module Dvda.Algorithm
            ) where

import Dvda.AD ( rad )
--import Dvda.CSE ( cse )
import Dvda.Expr ( Expr, sym, symDependent, symDependentN )
import Dvda.Algorithm
--import Dvda.Vis ( previewGraph, previewGraph' )
--import Dvda.HSBuilder
