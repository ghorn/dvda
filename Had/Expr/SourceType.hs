-- SourceType.hs

{-# OPTIONS_GHC -Wall #-}

module Had.Expr.SourceType( SourceType(..)
                          ) where

data (Show a, Eq a) => SourceType a = Number a
                                    | I Integer
                                    | Sym String
                                    | Zero deriving Eq

instance (Show a, Eq a) => Show (SourceType a) where
  show (Number a) = show a
  show (I i) = show i
  show (Sym s) = s
  show Zero = "0"
