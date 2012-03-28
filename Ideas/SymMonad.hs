{-# OPTIONS_GHC -Wall #-}

module Ideas.SymMonad( makeFun
                     , sym
                     , symVec
                     , symMat
                     , node
                     , FunGraph
                     ) where

import Control.Monad.State
import Data.Functor.Identity

import Ideas.Expr
import Ideas.Dim

type Key = Int

data FunGraph a = FunGraph [(Key, Expr a)] deriving (Show, Eq)

sym :: String -> State (FunGraph a) (Expr a)
sym = node . (ESym D0)

symVec :: Int -> String -> State (FunGraph a) (Expr a)
symVec d = node . (ESym (Dim [d]))

symMat :: (Int,Int) -> String -> State (FunGraph a) (Expr a)
symMat (r,c) = node . (ESym (Dim [r,c]))

node :: Expr a -> State (FunGraph a) (Expr a)
node expr = do
  (FunGraph xs) <- get
  let k = length xs
  put (FunGraph $ xs ++ [(k,expr)])
  return (ERef (dim expr) k)

makeFun :: State (FunGraph a) b -> (b, FunGraph a)
makeFun f = runState f (FunGraph [])

woo :: Num a => StateT (FunGraph a) Identity [Expr a]
woo = do
  x <- sym "x"
  let y = abs x
  z <- node (x*y)
  return [z, z*y]

run :: Num a => ([Expr a], FunGraph a)
run = makeFun woo
