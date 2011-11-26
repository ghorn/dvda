-- ElemwiseType.hs

{-# OPTIONS_GHC -Wall #-}

module Numeric.Dvda.Expr.ElemwiseType( ElemwiseType(..)
                                     , applyElemwise
                                     ) where

data ElemwiseType = Abs
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

applyElemwise :: Floating a => ElemwiseType -> (a -> a)
applyElemwise Abs    = abs   
applyElemwise Signum = signum
applyElemwise Neg    = \x -> -x
applyElemwise Exp    = exp
applyElemwise Sqrt   = sqrt
applyElemwise Log    = log
applyElemwise Sin    = sin
applyElemwise Cos    = cos
applyElemwise Tan    = tan
applyElemwise ASin   = asin
applyElemwise ACos   = acos
applyElemwise ATan   = atan
applyElemwise Sinh   = sinh
applyElemwise Cosh   = cosh
applyElemwise Tanh   = tanh
applyElemwise ASinh  = asinh
applyElemwise ACosh  = acosh
applyElemwise ATanh  = atanh

instance Show ElemwiseType where
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
