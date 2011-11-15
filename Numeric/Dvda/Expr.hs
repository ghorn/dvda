-- Expr.hs

{-# OPTIONS_GHC -Wall #-}

module Numeric.Dvda.Expr( Expr(..)
                        , Op2Type(..)
                        , ElemwiseType(..)
                        , SourceType(..)
                        , elemwisePrePost
                        , sym
                        , exprToGraph
                        , exprExample
                        , previewGraph
                        ) where

import Numeric.Dvda.Expr.Expr(Expr(..), sym)
import Numeric.Dvda.Expr.Op2Type(Op2Type(..))
import Numeric.Dvda.Expr.ElemwiseType(ElemwiseType(..), elemwisePrePost)
import Numeric.Dvda.Expr.SourceType(SourceType(..))
import Numeric.Dvda.Expr.ExprToGraph(exprToGraph, previewGraph)
import Numeric.Dvda.Expr.ExprExample(exprExample)
