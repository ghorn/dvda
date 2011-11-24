-- Fad.hs

{-# OPTIONS_GHC -Wall #-}

module Numeric.Dvda.AD.Fad( fad
                          , Dual(..)
                          ) where

import Data.Ratio(numerator, denominator)

data Dual a = Dual a a deriving (Show, Eq)

instance Num a => Num (Dual a) where
  (Dual x x') * (Dual y y') = Dual (x * y) (x*y' + x'*y)
  (Dual x x') + (Dual y y') = Dual (x + y) (x' + y')
  (Dual x x') - (Dual y y') = Dual (x - y) (x' - y')
  negate (Dual x x') = Dual (-x) (-x')
  abs (Dual x _) = Dual (abs x) (signum x)
  signum (Dual x _) = Dual (signum x) 0
  fromInteger x = Dual (fromInteger x) 0
  
instance Fractional a => Fractional (Dual a) where
  (Dual x x') / (Dual y y') = Dual (x/y) (x'/y - x/(y*y)*y')
  fromRational x = num/den
    where
      num = fromIntegral $ numerator x
      den = fromIntegral $ denominator x

instance Floating a => Floating (Dual a) where
  pi = Dual pi 0
  
  exp (Dual x x')  = Dual (exp x) ((exp x)*x')
  sqrt (Dual x x') = Dual (sqrt x) (x'/(2*(sqrt x)))
  log (Dual x x')  = Dual (log x) (x'/x)
  
  (Dual x x')**(Dual y y') = Dual (x**y) $ ( x'*y + x*y'*(log x) ) * x**(y-1)
  logBase (Dual b b') (Dual e e') = Dual (logBase b e) $
                                    (b*e'*(log b) - b'*e*log(e)) / (b*e*(log b)*(log b))
  
  sin (Dual x x')  = Dual (sin x) $ (cos x)*x'
  cos (Dual x x')  = Dual (cos x) $ -(sin x)*x'
  tan (Dual x x')  = Dual (tan x) $ x'/((cos x)*(cos x))
  
  asin (Dual x x') = Dual (asin x) $ x'/(sqrt (1 - x*x))
  acos (Dual x x') = Dual (acos x) $ -x'/(sqrt (1 - x*x))
  atan (Dual x x') = Dual (atan x) $ x'/(1 + x*x)
                     
  sinh (Dual x x')  = Dual (sinh x) $ (cosh x)*x'
  cosh (Dual x x')  = Dual (cosh x) $ (sinh x)*x'
  tanh (Dual x x')  = Dual (tanh x) $ x'/((cosh x)*(cosh x))
  
  asinh (Dual x x') = Dual (asinh x) $ x'/(sqrt (1 + x*x))
  acosh (Dual x x') = Dual (acosh x) $ x'/((sqrt (x - 1))*(sqrt (x + 1)))
  atanh (Dual x x') = Dual (atanh x) $ x'/(1 - x*x)

fad :: Num a => (Dual a -> [Dual b]) -> a -> [b]
fad f x = map pert $ f (Dual x 1)
  where
    pert (Dual _ p) = p
