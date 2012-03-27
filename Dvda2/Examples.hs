{-# OPTIONS_GHC -Wall #-}

module Numeric.Dvda.Symbolic.Examples( woo
                                     , run
                                     ) where

import Numeric.Dvda.Symbolic.SymMonad
import Numeric.Dvda.Symbolic.Expr

import Control.Monad.State
import Data.Functor.Identity

woo :: Num a => StateT (FunGraph a) Identity [Expr a]
woo = do
  x <- sym "x"
  let y = abs x
  z <- node (x*y)
  return [z, z*y]

run :: Num a => ([Expr a], FunGraph a)
run = makeFun woo
