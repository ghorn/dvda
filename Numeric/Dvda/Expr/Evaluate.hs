-- Evaluate.hs

{-# OPTIONS_GHC -Wall #-}

module Numeric.Dvda.Expr.Evalute( evaluate
                                ) where

import Numeric.Dvda.Expr.Expr
import Numeric.Dvda.Expr.Scalar
import Numeric.Dvda.Expr.UnaryType

-- evaluate all operations
evaluate :: Floating a => Expr a -> Expr a
evaluate x@(Sym _ _) = x
evaluate x@(EScalar _) = x
evaluate x@(Vector _ _) = x
evaluate x@(Matrix _ _) = x
evaluate (Broadcast (Vec n) (EScalar x)) = vec $ replicate n (evalScalar x)
evaluate (Broadcast (Mat (r,c)) (EScalar x)) = mat (r,c) $ replicate (r*c) (evalScalar x)
evaluate x@(Broadcast _ _) = x
evaluate (Unary ut arg) = applyUnary ut (evaluate arg)
evaluate (Binary bt arg1 arg2) = zipBinaryF bt (evaluate arg1) (evaluate arg2)
