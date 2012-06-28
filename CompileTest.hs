{-# OPTIONS_GHC -Wall #-}
{-# Language GADTs #-}
{-# Language FlexibleContexts #-}
{-# Language TypeOperators #-}
{-# Language TypeFamilies #-}

import Dvda

import Numeric.LinearAlgebra ( fromList, fromLists )

fun0 :: Exprs (DIM0 :* DIM1 :* DIM2) Double -> Exprs (DIM2 :* DIM1 :* DIM0) Double
fun0 (x :* y :* z) = z1 :* z2 :* z3
  where
    z1 = (scale x z)**3
--    z2 = (dot z y)**2
    z2 = y**2
    z3 = diff ((x*x/2)**x) x
--    z3 = ((x*x/2)**x)*x
  
main :: IO ()
main = do
  let x' = sym "x"
      y' = vsym 3 "y"
      z' = msym (2,3) "Z"
  
  fun <- buildHSFunctionPure fun0 (x' :* y' :* z')
  let x = 0.2
      y = fromList [0,1,2]
      z = fromLists [[0,1,2],[3,4,5]]
      answer = fun (x :* y :* z)
  
  print answer


fun1 :: Exprs (DIM0 :* DIM0 :* DIM0) Double -> Exprs (DIM0 :* DIM0 :* DIM0 :* DIM0) Double
fun1 (x :* y :* z) = f :* fx :* fy :* fz
  where
    f0 x' y' z' = (z' + x'*y')*log(cos x' / tanh y')**(z'/exp y')
    fx0 = f0 (f0 x y z) (f0 z y x) (f0 y x z)
    fy0 = f0 (f0 z x y) (f0 x z y) (f0 z z y)
    fz0 = f0 (f0 x y z) (f0 x y x) (f0 y x y)
    f = f0 fx0 fy0 fz0
    
    fx = diff f x
    fy = diff f y
    fz = diff f z

main' :: IO ()
main' = do
  fun <- buildHSFunctionPure fun1 (sym "x" :* sym "y" :* sym "z")
  let x = 0
      y = 3
      z = 6

  print $ fun (x :* y :* z)


newtonSearch :: (Expr DIM0 Double -> Expr DIM0 Double) -> IO (Double -> Double -> (Double, Double, Double, [Double]))
newtonSearch f_ = do
  let x = sym "x" :: Expr DIM0 Double
      y = f_ x
      y' = diff y x
      y'' = diff y' x
  call <- buildHSFunction x (y :* y' :* y'')
  
  let newtonSolver xs err = if (abs f') < err || length xs > 10
                            then (f, f', f'', x1:xs)
                            else newtonSolver (x1:xs) err
        where
          x0 = head xs
          x1 = x0 - f'/f''
          (f :* f' :* f'') = call x0
  return (\err x0 -> newtonSolver [x0] err)
