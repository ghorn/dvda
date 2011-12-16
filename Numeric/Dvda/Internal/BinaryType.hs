-- BinaryType.hs

{-# OPTIONS_GHC -Wall #-}

module Numeric.Dvda.Internal.BinaryType( BinaryType(..)
                                       , applyBinary
                                       ) where

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
