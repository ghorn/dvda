-- Op2Type.hs

{-# OPTIONS_GHC -Wall #-}

module Had.Expr.Op2Type( Op2Type(..)
                       ) where

data Op2Type = Mul
--             | Div
             | Add
             | Sub deriving Eq
instance Show Op2Type where
  show Mul = "*"
--  show Div = "/"
  show Add = "+"
  show Sub = "-"
