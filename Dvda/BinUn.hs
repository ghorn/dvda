{-# OPTIONS_GHC -Wall #-}
{-# Language TemplateHaskell #-}

module Dvda.BinUn ( BinOp(..)
                  , UnOp(..)
                  , showBinaryOperator
                  , showUnaryOperator
                  , applyUnary
                  , applyBinary
                  , unaryDeriv
                  , binaryDeriv
                  , isCommutative
                  , binPrec
                  ) where

import Data.Hashable ( Hashable, hash )
import Test.QuickCheck ( Arbitrary(..), oneof )
import FileLocation ( err )

import Dvda.Dual ( Dual(..), dualPerturbation )

data UnOp = Abs
          | Neg
          | Signum
          | Exp
          | Sqrt
          | Log
          | Sin
          | Cos
          | Tan
          | ASin
          | ACos
          | ATan
          | Tanh
          | Sinh
          | Cosh
          | ATanh
          | ASinh
          | ACosh deriving (Eq, Show, Enum, Bounded)

data BinOp = Add
           | Sub
           | Mul
           | Div
           | Pow
           | LogBase deriving (Eq, Show, Enum, Bounded)

-- Arbitrary math operations
instance Arbitrary UnOp where
    arbitrary = oneof $ map return [minBound..maxBound]

instance Arbitrary BinOp where
    arbitrary = oneof $ map return [minBound..maxBound]

instance Hashable UnOp where
  hash Abs    = 0
  hash Neg    = 1
  hash Signum = 2
  hash Exp    = 3
  hash Sqrt   = 4
  hash Log    = 5
  hash Sin    = 6
  hash Cos    = 7
  hash Tan    = 8
  hash ASin   = 9
  hash ACos   = 10
  hash ATan   = 11
  hash Tanh   = 12
  hash Sinh   = 13
  hash Cosh   = 14
  hash ATanh  = 15
  hash ASinh  = 16
  hash ACosh  = 17

instance Hashable BinOp where
  hash Add     = 18
  hash Sub     = 19
  hash Mul     = 20
  hash Div     = 21
  hash Pow     = 22
  hash LogBase = 23
                              
showUnaryOperator :: UnOp -> String
showUnaryOperator Abs    = "abs"
showUnaryOperator Neg    = "-"
showUnaryOperator Signum = "signum"
showUnaryOperator Exp    = "exp"
showUnaryOperator Sqrt   = "sqrt"
showUnaryOperator Log    = "log"
showUnaryOperator Sin    = "sin"
showUnaryOperator Cos    = "cos"
showUnaryOperator Tan    = "tan"
showUnaryOperator ASin   = "asin"
showUnaryOperator ACos   = "acos"
showUnaryOperator ATan   = "atan"
showUnaryOperator Sinh   = "sinh"
showUnaryOperator Cosh   = "cosh"
showUnaryOperator Tanh   = "tanh"
showUnaryOperator ASinh  = "asinh"
showUnaryOperator ATanh  = "atanh"
showUnaryOperator ACosh  = "acosh"

applyUnary :: Floating a => UnOp -> a -> a
applyUnary Abs    = abs
applyUnary Neg    = negate
applyUnary Signum = signum
applyUnary Exp    = exp
applyUnary Sqrt   = sqrt
applyUnary Log    = log
applyUnary Sin    = sin
applyUnary Cos    = cos
applyUnary Tan    = tan
applyUnary ASin   = asin
applyUnary ACos   = acos
applyUnary ATan   = atan
applyUnary Sinh   = sinh
applyUnary Cosh   = cosh
applyUnary Tanh   = tanh
applyUnary ASinh  = asinh
applyUnary ATanh  = atanh
applyUnary ACosh  = acosh

applyBinary :: Floating a => BinOp -> a -> a -> a
applyBinary Add = (+)
applyBinary Sub = (-)
applyBinary Mul = (*)
applyBinary Div = (/)
applyBinary Pow = (**)
applyBinary LogBase = logBase

unaryDeriv :: Floating a => UnOp -> (a,a) -> a
unaryDeriv op (x,x') = dualPerturbation $ applyUnary op (Dual x x')

binaryDeriv :: Floating a => BinOp -> (a,a) -> (a,a) -> a
binaryDeriv op (x,x') (y,y') = dualPerturbation $ applyBinary op (Dual x x') (Dual y y')

showBinaryOperator :: BinOp -> String
showBinaryOperator Add = "+"
showBinaryOperator Sub = "-"
showBinaryOperator Mul = "*"
showBinaryOperator Div = "/"
showBinaryOperator Pow = "**"
showBinaryOperator LogBase = "`logbase`"

isCommutative :: BinOp -> Bool
isCommutative Add     = True
isCommutative Sub     = False
isCommutative Mul     = True
isCommutative Div     = False
isCommutative Pow     = False
isCommutative LogBase = False

binPrec :: BinOp -> Int
binPrec Add = 6
binPrec Sub = 6
binPrec Mul = 7
binPrec Div = 7
binPrec Pow = 8
binPrec LogBase = $(err "logBase is not an infix operator")

paren :: String -> String
paren x = "( "++ x ++" )"
