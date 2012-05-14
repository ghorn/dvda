{-# OPTIONS_GHC -Wall #-}

module Ideas.BinUn ( BinOp(..)
                   , UnOp(..)
                   , showBinary
                   , showUnary
                   , applyUnary
                   , applyBinary
                   , isCommutative
                   ) where

import Data.Hashable(Hashable,hash)

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
                              
showUnary :: Show a => a -> UnOp -> String
showUnary x Abs    = '|': show x ++ "|"
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

paren :: Show a => a -> String
paren x = "( "++show x++" )"
