{-# OPTIONS_GHC -Wall #-}

module Ideas.Examples( woo
                     , run
                     ) where

import Ideas.SymMonad
import Ideas.Expr

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
