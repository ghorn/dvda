{-# OPTIONS_GHC -Wall #-}
{-# Language TypeOperators #-}

module Dvda.Examples ( exampleFun
                     , run
                     , run'
                     , showoff
                     ) where

import Control.Monad.State (State)
import Data.Array.Repa (DIM0,DIM1,DIM2)

import Dvda.SymMonad
import Dvda.Expr
import Dvda.Graph

exampleFun :: State (FunGraph Double (DIM0 :* DIM1 :* DIM2) (DIM2 :* DIM1 :* DIM0)) ()
exampleFun = do
  let x = sym "x"
      y = vsym 5 "y"
      z = msym (3,5) "Z"
  inputs_ (x :* y :* z)
  
  z1 <- node $ (scale x z)**3
  z2 <- node $ (dot z y)**2
  z3 <- node $ diff ((x*x/2)**x) x
  
  outputs_ (z1 :* z2 :* z3)

exampleFun' :: State (FunGraph Double (DIM0 :* DIM1 :* DIM2) (DIM2 :* DIM1 :* DIM0)) ()
exampleFun' = do
  let x = sym "x"
      y = vsym 5 "y"
      z = msym (3,5) "Z"
      z1 = (scale x z)**3
      z2 = (dot z y)**2
      z3 = diff ((x*x/2)**x) x

  inputs_ (x :* y :* z)
  outputs_ (z1 :* z2 :* z3)

run' :: IO ()
run' = do
  let gr :: FunGraph Double (DIM0 :* DIM1 :* DIM2) (DIM2 :* DIM1 :* DIM0)
      gr@(FunGraph hm im _ _) = runFunGraph exampleFun
      (FunGraph hm' im' _ _) = runFunGraph exampleFun'
      
  putStrLn $ funGraphSummary gr
  putStrLn $ showCollisions gr
  previewGraph gr
  putStrLn "\nimperative same as pure+cse?:"
  print $ hm == hm'
  print $ im == im'

run :: IO ()
run = do
  let gr :: FunGraph Double (DIM0 :* DIM0) (DIM0 :* DIM0)
      gr@( FunGraph _ _ _ _) = runFunGraph $ do
        let x = sym "x"
            y = sym "y"
            z1 = x * y
            z2 = diff z1 x

        inputs_ (x :* y)
        outputs_ (z1 :* z2)

  putStrLn $ showCollisions gr
  putStrLn "-------------------------------------------"
  putStrLn $ funGraphSummary gr
  putStrLn "-------------------------------------------"
  putStrLn $ funGraphSummary' gr
  previewGraph gr

showoff :: IO ()
showoff = do
  let gr :: FunGraph Double (DIM0 :* DIM0 :* DIM0) (DIM0 :* DIM0 :* DIM0 :* DIM0)
      gr = makeFunGraph (x' :* y' :* z') (f :* fx :* fy :* fz)
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


  putStrLn $ showCollisions gr
--  putStrLn $ funGraphSummary' gr
--  previewGraph gr
