{-# OPTIONS_GHC -Wall #-}

{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}
-- {-# LANGUAGE RebindableSyntax #-}

import Prelude hiding((+)) -- , fromInteger)
import qualified Prelude

class Addable a b c | a b -> c --, a c -> b, b c -> a where
  (+) :: a -> b -> c
infixl 6  +
  
data Matrix a = Matrix (Int,Int) [a]
data Vector a = Vector Int [a]
data Scalar a = Scalar a deriving (Eq, Show)

instance Num a => Addable a a a where
  (+) = (Prelude.+)

instance Num a => Addable (Scalar a) (Matrix a) (Matrix a) where
  (+) (Scalar x) (Matrix d xs) = Matrix d $ map (+ x) xs
instance Num a => Addable (Matrix a) (Scalar a) (Matrix a) where
  (+) m s = s + m

instance Num a => Addable (Scalar a) (Vector a) (Vector a) where
  (+) (Scalar x) (Vector d xs) = Vector d $ map (+ x) xs
instance Num a => Addable (Vector a) (Scalar a) (Vector a) where
  (+) v s = s + v

--instance Addable I I I where
--  (+) (I x) (I y) = I $ x + y
--
--instance Num a => Addable I (Scalar a) (Scalar a) where
--  (+) (I x) (Scalar y) = Scalar $ ((Prelude.fromInteger x) :: Num b => b) + y
--instance Num a => Addable (Scalar a) I (Scalar a) where
--  (+) x y = y + x


--data I = I Integer
--
--fromInteger :: Integer -> I
--fromInteger = I


f :: Num a => Vector a
f = 4 + (Vector 2 [5,6])



--instance Num a => Num (Scalar a) where
--  fromInteger a = 

--instance Num a => Addable a (Vector a) (Vector a) where
--  (+) x (Vector d xs) = Vector d $ map (+ x) xs
--instance Num a => Addable (Vector a) (Scalar a) (Vector a) where
--  (+) m s = s + m

--instance Num a => Addable (Scalar a) (Vector a) (Vector a) where
--  (+) (Scalar x) (Matrix y) = (Matrix x)


vec :: Num a => Int -> [a] -> Vector a
vec d xs
  | d == length xs = Vector d xs
  | otherwise      = error "vector constructor dim doesn't match length"

--mat :: (Int,Int) -> [a] -> Matrix a
--mat (r,c) xs
--  | r*c == length xs = Matrix (r,c) xs
--  | otherwise        = error "matrix constructor dims don't match length"

