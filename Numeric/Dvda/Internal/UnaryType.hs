-- UnaryType.hs

{-# OPTIONS_GHC -Wall #-}

module Numeric.Dvda.Internal.UnaryType( UnaryType(..)
                                      , applyUnary
                                      , cshow
                                      ) where

data UnaryType = Abs
               | Signum
               | Neg

               | Exp
               | Sqrt
               | Log

               | Sin
               | Cos
               | Tan

               | ASin
               | ACos
               | ATan

               | Sinh
               | Cosh
               | Tanh

               | ASinh
               | ACosh
               | ATanh deriving Eq 


applyUnary :: Floating a => UnaryType -> a -> a
applyUnary Abs    = abs   
applyUnary Signum = signum
applyUnary Neg    = \x -> -x
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
applyUnary ACosh  = acosh
applyUnary ATanh  = atanh


instance Show UnaryType where
  show Abs = "abs"
  show Signum = "signum"
  show Neg = "-"

  show Exp = "exp"
  show Sqrt = "sqrt"
  show Log = "log"

  show Sin = "sin"
  show Cos = "cos"
  show Tan = "tan"

  show ASin = "asin"
  show ACos = "acos"
  show ATan = "atan"

  show Sinh = "sinh"
  show Cosh = "cosh"
  show Tanh = "tanh"

  show ASinh = "asinh"
  show ACosh = "acosh"
  show ATanh = "atanh"

cshow :: UnaryType -> String
cshow Abs = "fabs"
cshow x = show x
