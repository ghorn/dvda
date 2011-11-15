-- ElemwiseType.hs

{-# OPTIONS_GHC -Wall #-}

module Numeric.Dvda.Expr.ElemwiseType( ElemwiseType(..)
                                     ) where

data ElemwiseType = Abs
                  | Signum
                  | Neg 
                  | Inv deriving Eq

instance Show ElemwiseType where
  show Abs = "abs"
  show Signum = "signum"
  show Neg = "-"
  show Inv = "inv"
