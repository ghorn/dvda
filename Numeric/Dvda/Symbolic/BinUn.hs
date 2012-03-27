{-# OPTIONS_GHC -Wall #-}

module Numeric.Dvda.Symbolic.BinUn( BinOp(..)
                                  , UnOp(..)
                                  , showBinary
                                  , showUnary
                                  , applyUnary
                                  , applyBinary
                                  ) where

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
          | ACosh deriving Eq

showUnary :: Show a => a -> UnOp -> String
showUnary x Abs    = "|" ++ show x ++ "|"
showUnary x Neg    = "-"++paren x
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

data BinOp = Add
           | Sub
           | Mul
           | Div
           | Pow
           | LogBase deriving Eq

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


paren :: Show a => a -> String
paren x = "( "++show x++" )"
