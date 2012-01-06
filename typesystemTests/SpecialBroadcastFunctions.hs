{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE Rank2Types, MultiParamTypeClasses, FlexibleInstances #-}

data D0 = D0
data D1 = D1
data D2 = D2
data D3 = D3

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
                | TNum [a]
                | TInt [Int]
                | TSym String 
                | TBroadcast (Tensor D0 a) deriving (Show, Eq)


safeBinaryApply :: Num a => (forall b . Num b => b -> b -> b) -> BinaryType -> Tensor d a -> Tensor d a -> Tensor d a
safeBinaryApply f _ (TNum xs) (TNum ys) = TNum $ zipWith f xs ys
safeBinaryApply f _ (TInt xs) (TInt ys) = TInt $ zipWith f xs ys
safeBinaryApply f _ (TInt xs) (TNum ys) = TNum $ zipWith f (map fromIntegral xs) ys
safeBinaryApply f _ (TNum xs) (TInt ys) = TNum $ zipWith f xs (map fromIntegral ys)

safeBinaryApply f _ (TBroadcast (TNum [tx])) (TNum ys) = TNum $ map (f tx) ys
safeBinaryApply f _ (TNum xs) (TBroadcast (TNum [ty])) = TNum $ map (\x -> f x ty) xs

safeBinaryApply f _ (TBroadcast (TInt [tx])) (TInt ys) = TInt $ map (f tx) ys
safeBinaryApply f _ (TInt xs) (TBroadcast (TInt [ty])) = TInt $ map (\x -> f x ty) xs

safeBinaryApply f _ (TBroadcast (TInt [tx])) (TNum ys) = TNum $ map (f (fromIntegral tx)) ys
safeBinaryApply f _ (TInt xs) (TBroadcast (TNum [ty])) = TNum $ map (\x -> f (fromIntegral x) ty) xs

safeBinaryApply f _ (TBroadcast (TNum [tx])) (TInt ys) = TNum $ map (f tx . fromIntegral) ys
safeBinaryApply f _ (TNum xs) (TBroadcast (TInt [ty])) = TNum $ map (\x -> f x (fromIntegral ty)) xs

safeBinaryApply _ binType x y = TBinary binType x y



safeUnaryApply :: Num a => (forall b . Num b => b -> b) -> UnaryType -> Tensor d a -> Tensor d a
safeUnaryApply f _ (TNum xs) = TNum $ map f xs
safeUnaryApply f _ (TInt xs) = TInt $ map f xs
safeUnaryApply f _ (TBroadcast tx) = TBroadcast $ f tx
safeUnaryApply _ unType tx = TUnary unType tx

instance Num a => Num (Tensor d a) where
  (*) tx ty
    | tIsI 1 tx = ty
    | tIsI 1 ty = tx
    | tIsI 0 tx = tx
    | tIsI 0 ty = ty
    | otherwise = safeBinaryApply (*) Mul tx ty

  (+) tx ty
    | tIsI 0 tx = ty
    | tIsI 0 ty = tx
    | otherwise = safeBinaryApply (+) Add tx ty

  (-) tx ty
    | tIsI 0 tx = negate ty
    | tIsI 0 ty = tx
    | otherwise = safeBinaryApply (-) Sub tx ty

  negate tx = safeUnaryApply negate Neg tx
  abs tx = safeUnaryApply abs Abs tx
  signum tx = safeUnaryApply signum Signum tx

  fromInteger x = TBroadcast (TNum [fromInteger x])


class Broadcastable d where
  bc :: Tensor D0 a -> Tensor d a
  
instance Broadcastable D0 where bc = id
instance Broadcastable D1 where bc = TBroadcast
instance Broadcastable D2 where bc = TBroadcast


--infixl 8 .**
infixl 7 .*, ./
infixl 6 .+, .-

(.*) :: (Broadcastable d, Num a) => Tensor D0 a -> Tensor d a -> (Tensor d a)
(.*) tx ty = (bc tx) * ty

(.+) :: (Broadcastable d, Num a) => Tensor D0 a -> Tensor d a -> (Tensor d a)
(.+) tx ty = (bc tx) + ty

(.-) :: (Broadcastable d, Num a) => Tensor D0 a -> Tensor d a -> (Tensor d a)
(.-) tx ty = (bc tx) - ty

(./) :: (Broadcastable d, Fractional a) => Tensor D0 a -> Tensor d a -> (Tensor d a)
(./) tx ty = (bc tx) / ty


-- | test if tensor is (or is broadcast from) SInt x
tIsI :: Int -> Tensor d a -> Bool
tIsI i (TInt [x]) = x == i
tIsI i (TBroadcast t) = tIsI i t
tIsI _ _ = False

  
instance Fractional a => Fractional (Tensor d a) where
  (/) (TNum x) (TNum y) = TNum $ zipWith (/) x y
  x / y 
    | tIsI 0 y  = error "Tensor divide by zero"
    | tIsI 0 x  = TInt [0]
    | otherwise = TBinary Div x y
  fromRational x = TNum [fromRational x]


t0 :: Tensor D0 Double
t0 = TNum [10]

t1 :: Tensor D1 Double
t1 = TNum [1,2,3]

t2 :: Tensor D2 Double
t2 = TNum [1,2,3]

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
