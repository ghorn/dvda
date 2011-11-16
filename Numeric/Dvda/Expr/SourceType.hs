-- SourceType.hs

{-# OPTIONS_GHC -Wall #-}

module Numeric.Dvda.Expr.SourceType( SourceType(..)
                                   , getSource
                                   ) where

data SourceType a = Number a
                  | I Integer
                  | Sym String
                  | Zero deriving Eq
                                  
getSource :: Num a => SourceType a -> a
getSource (Number x) = x
getSource (I x) = fromInteger x
getSource Zero = 0
getSource (Sym _) = error "can't getSource on a symbolic"

instance (Show a, Eq a) => Show (SourceType a) where
  show (Number a) = show a
  show (I i) = show i
  show (Sym s) = s
  show Zero = "0"
