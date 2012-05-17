{-# OPTIONS_GHC -Wall #-}
{-# Language GADTs #-}
{-# Language FlexibleContexts #-}
{-# Language TypeOperators #-}
{-# Language TypeFamilies #-}

module Dvda.Examples ( exampleFun
                     , run
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

run :: IO ()
run = do
  let gr :: FunGraph Double (DIM0 :* DIM1 :* DIM2) (DIM2 :* DIM1 :* DIM0)
      gr = snd $ makeFun exampleFun
  putStrLn $ funGraphSummary gr
  putStrLn $ showCollisions gr
  previewGraph gr
