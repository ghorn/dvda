-- Op2Type.hs

{-# OPTIONS_GHC -Wall #-}

module Had.Expr.Op2Type( Op2Type(..)
                       , op2DiffRule
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

-- op2 differentiation rules
op2DiffRule :: Num a => Op2Type -> (a, a) -> (a, a) -> a
op2DiffRule Mul (x, x') (y, y') = x*y' + x'*y
op2DiffRule Add (_, x') (_, y') = x' + y'
op2DiffRule Sub (_, x') (_, y') = x' - y'
