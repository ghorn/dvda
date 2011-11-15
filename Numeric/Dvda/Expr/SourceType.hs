-- SourceType.hs

{-# OPTIONS_GHC -Wall #-}

module Numeric.Dvda.Expr.SourceType( SourceType(..)
                                   , SourceConstant(..)
                                   ) where

data SourceType a = Number a
                  | I Integer
                  | Sym String
                  | Constant SourceConstant
                  | Zero deriving Eq
                                  
data SourceConstant = Pi deriving (Eq, Show)

instance (Show a, Eq a) => Show (SourceType a) where
  show (Number a) = show a
  show (I i) = show i
  show (Sym s) = s
  show Zero = "0"
  show (Constant Pi) = "pi"
