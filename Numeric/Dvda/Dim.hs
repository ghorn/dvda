-- Dim.hs

{-# OPTIONS_GHC -Wall #-}

module Numeric.Dvda.Dim( Dim(..)
                       , dorder
                       , dsize
                       ) where

--type Dim = [Int]
data Dim = D0 | Dim [Int] deriving Eq

instance Show Dim where
  show D0 = "D0"
  show (Dim d) = show d

-- order of a tensor dimension - 0 for scalar, 1 for vector, 2 or matrix, etc.
dorder :: Dim -> Int
dorder D0 = 0
dorder (Dim d) = length d

-- size of a tensor dimension
dsize :: Dim -> Int
dsize D0 = 1
dsize (Dim d) = product d
