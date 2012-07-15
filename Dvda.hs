{- |
   Module      : Dvda
   Description : Top level module

   This is the top level module which exports the API
 -}

{-# OPTIONS_GHC -Wall #-}
{-# Language TypeOperators #-}
{-# Language TypeFamilies #-}
{-# Language MultiParamTypeClasses #-}
{-# Language FlexibleInstances #-}

module Dvda ( -- * primitives
              sym
            , vsym
            , msym
            , svec
            , smat
            , vec
            , mat
              -- * operations
            , scale
--            , dot
            , diff
            , runDeriv
              -- * symbolic expression type
            , Expr
            , fullShow
            , fullShowNodes
              -- * construct FunGraphs
            , FunGraph
            , makeFunGraph
            , runFunGraph
            , inputs
            , outputs
            , inputs_
            , outputs_
            , node
              -- * show/summarize FunGraphs
            , funGraphSummary
            , funGraphSummary'
            , showCollisions
            , previewGraph
              -- * compile and link function
--            , buildHSFunction
--            , buildHSFunctionPure
--            , buildHSFunctionFromGraph
              -- * Heterogenous inputs/outputs
            , (:*)(..)
            , Exprs
              -- * re-export from repa and hmatrix
--            , Index
            , DIM0
            , DIM1
            , DIM2
            , Z(..)
            , (:.)
            , Matrix
            , Vector
            ) where

import Dvda.Expr
import Dvda.Graph
--import Dvda.HSBuilder
import Dvda.SymMonad

import Data.Array.Repa ( DIM0, DIM1, DIM2, Z(..), Shape, (:.) )
import Numeric.LinearAlgebra ( Matrix, Vector )

-- | Just a nice way to write (Exprs (DIM0 :* DIM1 :* DIM2) Double)
-- | instead of (Expr DIM0 Double :* Expr DIM1 Double :* Expr DIM2 Double)
class ExprList sh a where
  type Exprs sh a
  
instance (ExprList sh0 a, ExprList sh1 a) => ExprList (sh0 :* sh1) a where
  type Exprs (sh0 :* sh1) a = (Exprs sh0 a) :* (Exprs sh1 a)
      
instance ExprList Z a where
  type Exprs Z a = Expr Z a

instance Shape sh => ExprList (sh :. Int) a where
  type Exprs (sh :. Int) a = Expr (sh :. Int) a
