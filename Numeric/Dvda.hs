{- |
   Module      : Numeric.Dvda
   Description : Top level module

   This is the top level module which exports the API
 -}

{-# OPTIONS_GHC -Wall #-}

module Numeric.Dvda( -- * Core API
                     module Numeric.Dvda.Symbolic
                   , Expr
                     -- * Automatic differentiation
                   , module Numeric.Dvda.AD
                     -- * Visualization
                   , module Numeric.Dvda.Vis
                     -- * Turn expressions into callable functions
                   , module Numeric.Dvda.Function
                     -- * Utility functions
                   , showCSource
                   ) where

import Numeric.Dvda.AD
import Numeric.Dvda.Symbolic
import Numeric.Dvda.Vis
import Numeric.Dvda.Function
import Numeric.Dvda.Internal.Expr(Expr)
import Numeric.Dvda.Codegen.WriteC(showCSource)
