{-# OPTIONS_GHC -Wall #-}

module Numeric.Dvda.Symbolic.SymMonad( makeFun
                                     , sym
                                     , node
                                     , woo
                                     , run
                                     ) where

import Control.Monad.State
import Data.Functor.Identity

import Numeric.Dvda.Symbolic.Expr
import Numeric.Dvda.Symbolic.Dim

type Key = Int

data FunGraph a = FunGraph [(Key, Expr a)] deriving (Show, Eq)

freeKey :: FunGraph a -> Key
freeKey (FunGraph xs) = length xs

insert :: Key -> Expr a -> FunGraph a -> FunGraph a
insert k el (FunGraph xs) = FunGraph $ xs ++ [(k, el)]

sym :: String -> State (FunGraph a) (Expr a)
sym = node . (ESym D0)

node :: Expr a -> State (FunGraph a) (Expr a)
node expr = do
  gr <- get
  let k = freeKey gr
  put (insert k expr gr)
  return (ERef (dim expr) k)

makeFun :: State (FunGraph a) b -> FunGraph a
makeFun f = snd $ runState f (FunGraph [])

woo :: Num a => StateT (FunGraph a) Identity [Expr a]
woo = do
  x <- sym "x"
  let y = abs x
  z <- node (x*y)
  return [z, z*y]

run :: Num a => FunGraph a
run = makeFun woo
