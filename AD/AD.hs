-- AD.hs

{-# OPTIONS_GHC -Wall #-}

data Dual a = Dual a a deriving (Show, Eq)

instance Num a => Num (Dual a) where
  Dual a b + Dual c d = Dual (a+c) (b+d)
  Dual a b * Dual c d = Dual (a*c) (a*d + c*b)

diff :: (Show a, Num a) => (Dual a -> Dual a) -> a -> a
diff f x0 = (\(Dual _ d) -> d) $ f $ Dual x0 1

g :: (Num a) => a -> a
g x = x*x 

main :: IO ()
main = do
  print $ map (diff g) [0,1..3::Double]
