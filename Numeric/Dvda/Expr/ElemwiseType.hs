-- ElemwiseType.hs

{-# OPTIONS_GHC -Wall #-}

module Numeric.Dvda.Expr.ElemwiseType( ElemwiseType(..)
                                     , elemwiseDiffRule
                                     ) where

import Debug.Trace

data ElemwiseType = Abs
                  | Signum
                  | Negate deriving (Show, Eq)

-- elementwise differentiation rules
elemwiseDiffRule :: Num a => ElemwiseType -> (a, a) -> a
elemwiseDiffRule Abs (x,_) = signum x
elemwiseDiffRule Negate (_,x') = -x'
elemwiseDiffRule Signum _ = trace "MESSAGE: derivitive of signum taken to be 0, not dirac delta" 0
instance Show ElemwiseType where
  show Abs = "abs"
  show Signum = "signum"
