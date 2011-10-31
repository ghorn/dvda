-- ElemwiseType.hs

{-# OPTIONS_GHC -Wall #-}

module Had.Expr.ElemwiseType( ElemwiseType(..)
                            , elemwisePrePost
                            ) where

data ElemwiseType = Abs
                  | Signum
                  | Negate deriving (Show, Eq)

elemwisePrePost :: ElemwiseType -> (String, String)
--elemwisePrePost Abs = ("|", "|")
elemwisePrePost Abs = ("abs", "")
elemwisePrePost Signum = ("signum", "")
elemwisePrePost Negate = ("negate", "")
