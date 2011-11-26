-- Op2Type.hs

{-# OPTIONS_GHC -Wall #-}

module Numeric.Dvda.Expr.Op2Type( Op2Type(..)
                                , applyOp2
                                ) where

data Op2Type = Mul
             | Add
             | Div
             | Sub
             | Pow
             | LogBase deriving Eq

applyOp2 :: Floating a => Op2Type -> (a -> a -> a)
applyOp2 Mul     = (*)
applyOp2 Add     = (+)
applyOp2 Div     = (/)
applyOp2 Sub     = (-)
applyOp2 Pow     = (**)
applyOp2 LogBase = logBase

instance Show Op2Type where
  show Mul = "*"
  show Add = "+"
  show Div = "/"
  show Sub = "-"
  show Pow = "**"
  show LogBase = "`logBase`"
