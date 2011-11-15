-- Op2Type.hs

{-# OPTIONS_GHC -Wall #-}

module Numeric.Dvda.Expr.Op2Type( Op2Type(..)
                                ) where

data Op2Type = Mul
             | Add
             | Sub deriving Eq
instance Show Op2Type where
  show Mul = "*"
  show Add = "+"
  show Sub = "-"
