-- Evaluate.hs

{-# OPTIONS_GHC -Wall #-}

module Numeric.Dvda.Expr.Evalute( evaluate
                                ) where

import qualified Data.Array.Repa as R
import Data.Array.Repa((:.)(..))
import Debug.Trace

import Numeric.Dvda.Expr.Expr
import Numeric.Dvda.Expr.Scalar
import Numeric.Dvda.Expr.UnaryType
import Numeric.Dvda.Expr.BinaryType

-- evaluate all operations
{-# INLINE evaluate #-}
evaluate :: (Floating a, R.Shape sh, R.Elt a) => Expr sh a -> R.Array sh a
evaluate (Sym {}) = error "can't evaluate symbolic"
evaluate (EScalar _) = error "should have broadcast EScalar" -- R.singleton (evalScalar s)
evaluate (Broadcast { arg' = a
                    , dim = d
                    }) = R.backpermute d (\_ -> R.Z) (R.singleton (evalScalar a))
evaluate (Tensor t) = t
evaluate (Unary { unaryType = ut
                , arg = a
                }) = R.map (applyUnary ut) (evaluate a)
evaluate (Binary { binaryType = bt
                 , arg1 = a1
                 , arg2 = a2
                 }) = R.zipWith (applyBinary bt) (evaluate a1) (evaluate a2)
evaluate (Dot { arg1' = a1
              , arg2' = a2
              , dim = d
              }) 
  | d == R.extent answer = trace "disable this Shape assert in Dvda.Expr.Evalaute" answer 
  | otherwise            = error "Shape assert fail in Dvda.Expr.Evaluate"
  where
    answer = applyDot (evaluate a1) (evaluate a2)
