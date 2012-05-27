{-# OPTIONS_GHC -Wall #-}
{-# Language GADTs #-}
{-# Language FlexibleContexts #-}
{-# Language TypeOperators #-}
{-# Language TypeFamilies #-}

import Data.Array.Repa (DIM0,DIM1,DIM2)

import Dvda

gr' :: FunGraph Double (DIM0 :* DIM0 :* DIM0) (DIM0 :* DIM0 :* DIM0 :* DIM0)
gr' = makeFunGraph (x' :* y' :* z') (f :* fx :* fy :* fz)
  where
    x' = sym "x"
    y' = sym "y"
    z' = sym "z"

    f0 x y z = (z + x*y)*log(cos x / tanh y)**(z/exp y)
    fx0 = f0 (f0 x' y' z') (f0 z' y' x') (f0 y' x' z')
    fy0 = f0 (f0 z' x' y') (f0 x' z' y') (f0 z' z' y')
    fz0 = f0 (f0 x' y' z') (f0 x' y' x') (f0 y' x' y')
    f = f0 fx0 fy0 fz0
    
    fx = diff f x'
    fy = diff f y'
    fz = diff f z'


gr :: FunGraph Double (DIM0 :* DIM1 :* DIM2) (DIM2 :* DIM1 :* DIM0)
gr = runFunGraph $ do
  let x = sym "x"
      y = vsym 3 "y"
      z = msym (2,3) "Z"
  inputs_ (x :* y :* z)
  
  z1 <- node $ (scale x z)**3
  z2 <- node $ (dot z y)**2
--  z2 <- node $ (z*y)**2
--  z3 <- node $ diff ((x*x/2)**x) x
  z3 <- node $ ((x*x/2)**x)*x
  
  outputs_ (z1 :* z2 :* z3)

main :: IO ()
main = do
  fun <- buildHSFunction gr
  let x = 0
      y = vec [0,1,2]
      z = mat (2,3) [[0,1,2],[3,4,5]]
      answer = fun (x :* y :* z)
  
  print answer

main' :: IO ()
main' = do
  fun <- buildHSFunction gr'
  let x = 0
      y = 3
      z = 6

  print $ fun (x :* y :* z)
