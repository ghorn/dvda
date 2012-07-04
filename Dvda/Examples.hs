{-# OPTIONS_GHC -Wall #-}
{-# Language TypeOperators #-}

module Dvda.Examples ( run
                     , run'
                     , showoff
                     , bigGraph
                     , smallGraph
                     , runCallNative
                     ) where

import Data.Array.Repa.Index
import Control.Monad.State

import Dvda
import Dvda.Expr
import Dvda.CallNative
import Dvda.Graph ( FunGraph(..), fullShowNodes )

exampleFunGraph :: State (FunGraph
                          Double (Exprs (DIM0 :* DIM1 :* DIM2) Double)
                          (Exprs (DIM2 :* DIM1 :* DIM0) Double))
                          ()
exampleFunGraph = do
  let x = sym "x" :: Expr DIM0 Double
      y = vsym 5 "y"
      z = msym (3,5) "Z"
  inputs_ (x :* y :* z)
  
  z1 <- node $ (scale x z)**3
--  z2 <- node $ (dot z y)**2
  z2 <- node $ y**2
  z3 <- node $ diff ((x*x/2)**x) x
  
  outputs_ (z1 :* z2 :* z3)

pureFun :: Exprs (DIM0 :* DIM1 :* DIM2) Double -> Exprs (DIM2 :* DIM1 :* DIM0) Double
pureFun (x :* y :* z) = z1 :* z2 :* z3
  where
    z1 = (scale x z)**3
--    z2 = (dot z y)**2
    z2 = y**2
    z3 = diff ((x*x/2)**x) x

exampleFunGraph' :: State (FunGraph
                           Double
                           (Exprs (DIM0 :* DIM1 :* DIM2) Double)
                           (Exprs (DIM2 :* DIM1 :* DIM0) Double))
                    ()
exampleFunGraph' = do
  let x = sym "x" :: Expr DIM0 Double
      y = vsym 5 "y"
      z = msym (3,5) "Z"
      
      args = x :* y :* z
  
  inputs_ args
  outputs_ (pureFun args)

run' :: IO ()
run' = do
  let gr@(FunGraph hm im _ _) = runFunGraph exampleFunGraph
      (FunGraph hm' im' _ _) = runFunGraph exampleFunGraph'
      
  putStrLn $ funGraphSummary' gr
  putStrLn $ showCollisions gr
  previewGraph gr
  putStrLn "\nimperative same as pure+cse?:"
  print $ hm == hm'
  print $ im == im'

run :: IO ()
run = do
  let gr@( FunGraph _ _ _ _) = runFunGraph $ do
        let x = sym "x" :: Expr DIM0 Double
            y = sym "y"
            z1 = x + x / y + 3
            z2 = diff z1 x
            z3 = diff z1 y

        inputs_ (x :* y)
        outputs_ (z1 :* z2 :* z3)

  putStrLn $ showCollisions gr
  putStrLn $ fullShowNodes gr
  let FunGraph _ _ _ (z:* zx :* zy) = gr
  putStrLn $ "\nz:     " ++ fullShow gr z
  putStrLn $ "dz/dx: " ++ fullShow gr zx
  putStrLn $ "dz/dy: " ++ fullShow gr zy
  previewGraph gr

bigGraph :: FunGraph Double
            (Exprs (DIM0 :* DIM0 :* DIM0) Double)
            (Exprs (DIM0 :* DIM0 :* DIM0 :* DIM0) Double)
bigGraph = makeFunGraph (x' :* y' :* z') (f :* fx :* fy :* fz)
  where
    x' = sym "x" :: Expr DIM0 Double
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

smallGraph :: FunGraph Double
            (Exprs (DIM0 :* DIM0 :* DIM0) Double)
            (Exprs (DIM0 :* DIM0) Double)
smallGraph = makeFunGraph (x :* y :* z) (f0 :* f1)
  where
    x = sym "x" :: Expr DIM0 Double
    y = sym "y"
    z = sym "z"

    f0 = x*y*z + 3
    f1 = 40*f0/x

runCallNative :: Exprs (Z :* Z) Double
runCallNative = toNative smallGraph (f 1 :* f 2 :* f 3)
  where
    f = EConst . (CSingleton Z)

showoff :: IO ()
showoff = do
  putStrLn $ showCollisions bigGraph
  let FunGraph _ _ _ (f :* fx :* fy :* fz) = bigGraph
  putStrLn "--------------------------------------------------------------"
  putStrLn $ fullShow bigGraph f
  putStrLn "--------------------------------------------------------------"
  putStrLn $ fullShow bigGraph fx
  putStrLn "--------------------------------------------------------------"
  putStrLn $ fullShow bigGraph fy
  putStrLn "--------------------------------------------------------------"
  putStrLn $ fullShow bigGraph fz
  putStrLn "--------------------------------------------------------------"
--  putStrLn $ funGraphSummary' bigGraph
  previewGraph bigGraph
