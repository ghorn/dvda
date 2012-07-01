{-# OPTIONS_GHC -Wall #-}

module Dvda.BinUn ( BinOp(..)
                  , UnOp(..)
                  , showBinary
                  , showUnary
                  , applyUnary
                  , applyBinary
                  , unaryDeriv
                  , binaryDeriv
                  , isCommutative
                  , lassoc
                  , rassoc
                  ) where

import Data.Hashable ( Hashable, hash )

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
          | ACosh deriving (Eq, Show)

data BinOp = Add
           | Sub
           | Mul
           | Div
           | Pow
           | LogBase deriving (Eq, Show)

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
                              
showUnary :: String -> UnOp -> String
showUnary x Abs    = '|': x ++ "|"
showUnary x Neg    = '-':paren x
showUnary x Signum = "signum"++paren x
showUnary x Exp    = "exp"++paren x
showUnary x Sqrt   = "sqrt"++paren x
showUnary x Log    = "log"++paren x
showUnary x Sin    = "sin"++paren x
showUnary x Cos    = "cos"++paren x
showUnary x Tan    = "tan"++paren x
showUnary x ASin   = "asin"++paren x
showUnary x ACos   = "acos"++paren x
showUnary x ATan   = "atan"++paren x
showUnary x Sinh   = "sinh"++paren x
showUnary x Cosh   = "cosh"++paren x
showUnary x Tanh   = "tanh"++paren x
showUnary x ASinh  = "asinh"++paren x
showUnary x ATanh  = "atanh"++paren x
showUnary x ACosh  = "acosh"++paren x

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

showBinary :: BinOp -> String
showBinary Add = "+"
showBinary Sub = "-"
showBinary Mul = "*"
showBinary Div = "/"
showBinary Pow = "**"
showBinary LogBase = "`logbase`"

isCommutative :: BinOp -> Bool
isCommutative Add     = True
isCommutative Sub     = False
isCommutative Mul     = True
isCommutative Div     = False
isCommutative Pow     = False
isCommutative LogBase = False

lassoc :: BinOp -> BinOp -> Bool
lassoc Add Add = True -- a + b + c == (a + b) + c
lassoc Add Sub = True -- a + b - c == (a + b) - c
--lassoc Add Mul = True -- a + b * c == (a + b) * c
--lassoc Add Div = True -- a + b / c == (a + b) / c

lassoc Sub Add = True -- a - b + c == (a - b) + c
lassoc Sub Sub = True -- a - b - c == (a - b) - c
--lassoc Sub Mul = True -- a - b * c == (a - b) * c
--lassoc Sub Div = True -- a - b / c == (a - b) / c

lassoc Div Add = True -- a / b + c == (a / b) + c
lassoc Div Sub = True -- a / b - c == (a / b) - c
lassoc Div Mul = True -- a / b * c == (a / b) * c
lassoc Div Div = True -- a / b / c == (a / b) / c

lassoc Mul Add = True -- a * b + c == (a * b) + c
lassoc Mul Sub = True -- a * b - c == (a * b) - c
lassoc Mul Mul = True -- a * b * c == (a * b) * c
lassoc Mul Div = True -- a * b / c == (a * b) / c

lassoc _ _ = False

rassoc :: BinOp -> BinOp -> Bool
--rassoc Add Add = True -- a + b + c == a + (b + c)
--rassoc Add Sub = True -- a + b - c == a + (b - c)
rassoc Add Mul = True -- a + b * c == a + (b * c)
rassoc Add Div = True -- a + b / c == a + (b / c)

--rassoc Sub Add = True -- a - b + c == a - (b + c)
--rassoc Sub Sub = True -- a - b - c == a - (b - c)
rassoc Sub Mul = True -- a - b * c == a - (b * c)
rassoc Sub Div = True -- a - b / c == a - (b / c)

--rassoc Div Add = True -- a / b + c == a / (b + c)
--rassoc Div Sub = True -- a / b - c == a / (b - c)
--rassoc Div Mul = True -- a / b * c == a / (b * c)
--rassoc Div Div = True -- a / b / c == a / (b / c)

--rassoc Mul Add = True -- a * b + c == a * (b + c)
--rassoc Mul Sub = True -- a * b - c == a * (b - c)
--rassoc Mul Mul = True -- a * b * c == a * (b * c)
--rassoc Mul Div = True -- a * b / c == a * (b / c)

rassoc _ _ = False

paren :: String -> String
paren x = "( "++ x ++" )"
