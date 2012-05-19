{-# OPTIONS_GHC -Wall #-}
{-# Language GADTs #-}
{-# Language FlexibleContexts #-}
{-# Language TypeOperators #-}
{-# Language TypeFamilies #-}

import qualified Data.Vector.Unboxed as V
import Data.Array.Repa (DIM0,DIM1,DIM2)

import Dvda.SymMonad ( (:*)(..), makeFun, inputs_, outputs_, node )
import Dvda.Expr
import Dvda.Graph
import Dvda.Codegen.HSBuilder

import Dvda

gr :: FunGraph Double (DIM0 :* DIM1 :* DIM2) (DIM2 :* DIM1 :* DIM0)
--gr :: FunGraph Double (DIM0 :* DIM0 :* DIM0) (DIM0 :* DIM0 :* DIM0)
gr = snd $ makeFun $ do
  let x = sym "x"
      y = vsym 3 "y"
--      y = sym "y"
      z = msym (2,3) "Z"
--      z = sym "Z"
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
      y = vec [0,1,2::Double]
      z = mat (2,3) [0,1,2,3,4,5]
      answer = fun (x :* y :* z)
      
  print answer
