-- ElemwiseType.hs

{-# OPTIONS_GHC -Wall #-}

module Numeric.Dvda.Expr.ElemwiseType( ElemwiseType(..)
                                     ) where

data ElemwiseType = Abs
                  | Signum
                  | Neg 
                  | Inv 
                    
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


instance Show ElemwiseType where
  show Abs = "abs"
  show Signum = "signum"
  show Neg = "-"
  show Inv = "inv"

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
