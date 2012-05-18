{-# OPTIONS_GHC -Wall #-}
{-# Language GADTs #-}
{-# Language FlexibleContexts #-}
{-# Language TypeOperators #-}
{-# Language TypeFamilies #-}

module Dvda.Codegen.CFunction ( run
                              ) where

--import Data.Array.Repa ( DIM0, DIM1, DIM2 )
import Data.Array.Repa ( DIM0 )

import Dvda.SymMonad
import Dvda.Expr
import Dvda.Graph
import Dvda.Codegen.CSyntax ( writeC )

--gr :: FunGraph Double (DIM0 :* DIM1 :* DIM2) (DIM2 :* DIM1 :* DIM0)
gr :: FunGraph Double (DIM0 :* DIM0 :* DIM0) (DIM0 :* DIM0 :* DIM0)
gr = snd $ makeFun $ do
  let x = sym "x"
--      y = vsym 5 "y"
      y = sym "y"
--      z = msym (3,5) "Z"
      z = sym "Z"
  inputs_ (x :* z :* y)
  
  z1 <- node $ (scale x z)**3
--  z2 <- node $ (dot z y)**2
  z2 <- node $ (z*y)**2
  z3 <- node $ diff ((x*x/2)**x) x
  
  outputs_ (z1 :* z2 :* z3)

run :: IO ()
run = do
  let (src, include) = writeC gr
  putStrLn src
  putStrLn include
--  previewGraph gr
