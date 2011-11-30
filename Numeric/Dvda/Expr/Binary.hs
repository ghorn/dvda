-- Binary.hs

{-# OPTIONS_GHC -Wall #-}

module Numeric.Dvda.Expr.Binary( Binary(..)
                               , BinaryType(..)
                               , applyBinary
                               ) where

data Binary a = Binary BinaryType a a deriving Eq

instance Show a => Show (Binary a) where
  show (Binary binaryType x y) = "( " ++ show x ++ " " ++ show binaryType ++ " " ++ show y ++ " )"

data BinaryType = Mul
                | Add
                | Div
                | Sub
                | Pow
                | LogBase deriving Eq

applyBinary :: Floating a => BinaryType -> a -> a -> a
applyBinary Mul     = (*)
applyBinary Add     = (+)
applyBinary Div     = (/)
applyBinary Sub     = (-)
applyBinary Pow     = (**)
applyBinary LogBase = logBase

instance Show BinaryType where
  show Mul = "*"
  show Add = "+"
  show Div = "/"
  show Sub = "-"
  show Pow = "**"
  show LogBase = "`logBase`"
