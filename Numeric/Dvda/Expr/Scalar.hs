-- Scalar.hs

{-# OPTIONS_GHC -Wall #-}

module Numeric.Dvda.Expr.Scalar( Scalar(..)
                               , evalScalar
                               ) where

import Data.Ratio((%))

evalScalar :: Num a => Scalar a -> a
evalScalar (N x) = x
evalScalar (I x) = fromIntegral x

data Scalar a = N a
              | I Int deriving (Eq, Show)
instance Num a => Num (Scalar a) where
  (N x) + (N y) = N $ x + y
  (I x) + (I y) = I $ x + y
  (N x) + (I y) = N $ x + (fromIntegral y)
  (I x) + (N y) = N $ (fromIntegral x) + y

  (N x) * (N y) = N $ x * y
  (I x) * (I y) = I $ x * y
  (N x) * (I y) = N $ x * (fromIntegral y)
  (I x) * (N y) = N $ (fromIntegral x) * y

  (N x) - (N y) = N $ x - y
  (I x) - (I y) = I $ x - y
  (N x) - (I y) = N $ x - (fromIntegral y)
  (I x) - (N y) = N $ (fromIntegral x) - y
  
  abs (N x) = N (abs x)
  abs (I x) = I (abs x)

  signum (N x) = N (signum x)
  signum (I x) = I (signum x)

  fromInteger x = I (fromInteger x)

instance Fractional a => Fractional (Scalar a) where
  (N x) / (N y) = N $ x / y
  (I x) / (I y) = N $ fromRational (toInteger x % toInteger y)
  (N x) / (I y) = N $ x / (fromIntegral y)
  (I x) / (N y) = N $ (fromIntegral x) / y

  fromRational x = N $ fromRational x
