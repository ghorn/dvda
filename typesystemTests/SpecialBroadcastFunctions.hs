{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE Rank2Types, MultiParamTypeClasses, FlexibleInstances, FlexibleContexts #-}

import Data.Array.Repa hiding (map, zipWith)
import qualified Data.Array.Repa as R

data BinaryType = Add
                | Sub
                | Mul
                | Div deriving (Eq, Show)

data UnaryType = Sin
               | Cos
               | Signum
               | Neg
               | Abs deriving (Eq, Show)

data Tensor d a = TBinary BinaryType (Tensor d a) (Tensor d a)
                | TUnary UnaryType (Tensor d a)
                | TNum (Array d a)
                | TInt (Array d Int)
                | TSym String 
                | TBroadcast (Tensor DIM0 a) deriving (Show, Eq)


safeBinaryApply :: (Shape d, Elt a, Num a)
                   => (a -> a -> a)
                   -> BinaryType
                   -> Tensor d a -> Tensor d a
                   -> Tensor d a
safeBinaryApply f _ (TNum xs) (TNum ys) = TNum $ R.zipWith f xs ys
safeBinaryApply f _ (TInt xs) (TNum ys) = TNum $ R.zipWith f (R.map fromIntegral xs) ys
safeBinaryApply f _ (TNum xs) (TInt ys) = TNum $ R.zipWith f xs (R.map fromIntegral ys)

safeBinaryApply f _ (TBroadcast (TNum xs)) (TNum ys) = TNum $ R.map (f (toScalar xs)) ys
safeBinaryApply f _ (TNum xs) (TBroadcast (TNum ys)) = TNum $ R.map (\x -> f x (toScalar ys)) xs

safeBinaryApply f _ (TBroadcast (TInt xs)) (TNum ys) = TNum $ R.map (f (fromIntegral (toScalar xs))) ys
safeBinaryApply f _ (TInt xs) (TBroadcast (TNum ys)) = TNum $ R.map (\x -> f (fromIntegral x) (toScalar ys)) xs

safeBinaryApply f _ (TBroadcast (TNum xs)) (TInt ys) = TNum $ R.map (f (toScalar xs) . fromIntegral) ys
safeBinaryApply f _ (TNum xs) (TBroadcast (TInt ys)) = TNum $ R.map (\x -> f x (fromIntegral (toScalar ys))) xs

safeBinaryApply _ binType x y = TBinary binType x y



safeUnaryApply :: (Shape d, Elt a, Num a) => (forall b . Num b => b -> b) -> UnaryType -> Tensor d a -> Tensor d a
safeUnaryApply f _ (TNum xs) = TNum $ R.map f xs
safeUnaryApply f _ (TInt xs) = TInt $ R.map f xs
safeUnaryApply f _ (TBroadcast tx) = TBroadcast $ f tx
safeUnaryApply _ unType tx = TUnary unType tx


instance (Shape d, Elt a, Num a, Broadcastable d) => Num (Tensor d a) where
  (*) (TInt xs) (TInt ys) = TInt $ R.zipWith (*) xs ys
  (*) (TBroadcast (TInt xs)) (TInt ys) = TInt $ R.map (toScalar xs *) ys
  (*) (TInt xs) (TBroadcast (TInt ys)) = TInt $ R.map (\x -> x * toScalar ys) xs
  (*) tx ty
    | tIsI 1 tx = ty
    | tIsI 1 ty = tx
    | tIsI 0 tx = tx
    | tIsI 0 ty = ty
    | otherwise = safeBinaryApply (*) Mul tx ty

  (+) (TInt xs) (TInt ys) = TInt $ R.zipWith (+) xs ys
  (+) (TBroadcast (TInt xs)) (TInt ys) = TInt $ R.map (toScalar xs +) ys
  (+) (TInt xs) (TBroadcast (TInt ys)) = TInt $ R.map (\x -> x + toScalar ys) xs
  (+) tx ty
    | tIsI 0 tx = ty
    | tIsI 0 ty = tx
    | otherwise = safeBinaryApply (+) Add tx ty

  (-) (TInt xs) (TInt ys) = TInt $ R.zipWith (-) xs ys
  (-) (TBroadcast (TInt xs)) (TInt ys) = TInt $ R.map (toScalar xs -) ys
  (-) (TInt xs) (TBroadcast (TInt ys)) = TInt $ R.map (\x -> x - toScalar ys) xs
  (-) tx ty
    | tIsI 0 tx = negate ty
    | tIsI 0 ty = tx
    | otherwise = safeBinaryApply (-) Sub tx ty

  negate = safeUnaryApply negate Neg
  abs = safeUnaryApply abs Abs
  signum = safeUnaryApply signum Signum

  fromInteger x = bc $ TInt $ singleton (fromInteger x)


class Broadcastable d where
  bc :: Tensor DIM0 a -> Tensor d a
  bcIsI :: Int -> Tensor d a -> Bool
  bcIsI _ _ = False

instance Broadcastable DIM0 where
  bc = id
  bcIsI i (TInt xs) = i == toScalar xs
  bcIsI _ _ = False
  
instance Broadcastable DIM1 where bc = TBroadcast
instance Broadcastable DIM2 where bc = TBroadcast


--infixl 8 .**
infixl 7 .*, ./
infixl 6 .+, .-

(.*) :: (Broadcastable d, Num (Tensor d a)) => Tensor DIM0 a -> Tensor d a -> Tensor d a
(.*) tx ty = bc tx * ty

(.+) :: (Broadcastable d, Num (Tensor d a)) => Tensor DIM0 a -> Tensor d a -> Tensor d a
(.+) tx ty = bc tx + ty

(.-) :: (Broadcastable d, Num (Tensor d a)) => Tensor DIM0 a -> Tensor d a -> Tensor d a
(.-) tx ty = bc tx - ty

(./) :: (Broadcastable d, Fractional (Tensor d a)) => Tensor DIM0 a -> Tensor d a -> Tensor d a
(./) tx ty = bc tx / ty


-- | test if tensor is (or is broadcast from) SInt x
tIsI :: Broadcastable d => Int -> Tensor d a -> Bool
tIsI i (TBroadcast t) = tIsI i t
tIsI i tx = bcIsI i tx


instance (Shape d, Elt a, Fractional a, Broadcastable d) => Fractional (Tensor d a) where
  (/) (TInt xs) (TInt ys) = TNum $ R.zipWith (\x y -> fromIntegral x / fromIntegral y) xs ys
  (/) (TBroadcast (TInt xs)) (TInt ys) = TNum $ R.map (\y -> fromIntegral (toScalar xs) / fromIntegral y) ys
  (/) (TInt xs) (TBroadcast (TInt ys)) = TNum $ R.map (\x -> fromIntegral x / fromIntegral (toScalar ys)) xs
  (/) tx ty
    | tIsI 0 ty = error "Tensor divide by zero"
    | tIsI 0 tx = tx
    | otherwise = safeBinaryApply (/) Div tx ty

  fromRational x = bc $ TNum $ singleton (fromRational x)


sca :: Elt a => a -> Tensor DIM0 a
sca = TNum . singleton

vec :: Elt a => [a] -> Tensor DIM1 a
vec xs = TNum $ fromList (Z :. length xs) xs

mat :: Elt a => (Int, Int) -> [a] -> Tensor DIM2 a
mat (ix,iy) xs
  | ix*iy == length xs = TNum $ fromList (Z :. ix :. iy) xs
  | otherwise          = error "in \"mat\": dimension mismatch"

t0 :: Tensor DIM0 Double
t0 = sca 10

t1 :: Tensor DIM1 Double
t1 = vec [1,2,3]

t2 :: Tensor DIM2 Double
t2 = mat (2,3) [1,2,3,4,5,6]

doMults :: IO ()
doMults = do
  let g00 = t0 * t0
      g11 = t1 * t1
      g01 = t0 .* t1
      g02 = t0 .* t2

  print g00
  print g11
  print g01
  print g02
