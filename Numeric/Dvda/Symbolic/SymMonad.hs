{-# OPTIONS_GHC -Wall #-}
{-# Language FlexibleContexts #-}

module Numeric.Dvda.Symbolic.SymMonad( makeFun
                                     , sym
                                     , outputs
                                     , node
                                     , woo
                                     , run
                                     ) where

import Control.Monad.State
import Data.Functor.Identity

import Numeric.Dvda.Symbolic.BinUn

type Key = Int

data FunGraph a = FunGraph [(Key, Expr a)] deriving (Show, Eq)

data Expr a = Sym String
            | Binary BinOp (Expr a) (Expr a)
            | Unary UnOp (Expr a)
            | Const a
            | Output (Expr a)
            | Ref Key deriving (Show, Eq)

instance Num a => Num (Expr a) where
  (*) = Binary Mul
  (+) = Binary Add
  (-) = Binary Sub
  abs = Unary Abs
  negate = Unary Neg
  signum = Unary Signum
  fromInteger = Const . fromInteger

--newFunGraph = FunGraph []

freeKey :: FunGraph a -> Key
freeKey (FunGraph xs) = length xs

insert :: Key -> Expr a -> FunGraph a -> FunGraph a
insert k el (FunGraph xs) = FunGraph $ xs ++ [(k, el)]

sym :: String -> State (FunGraph a) (Expr a)
sym = node . Sym

outputs :: [Expr a] -> StateT (FunGraph a) Identity [Expr a]
outputs = mapM (node . Output)

node :: Expr a -> State (FunGraph a) (Expr a)
node expr = do
  gr <- get
  let k = freeKey gr
  put (insert k expr gr)
  return (Ref k)

makeFun :: State (FunGraph a) b -> FunGraph a
makeFun f = snd $ runState f (FunGraph [])

woo :: Num a => StateT (FunGraph a) Identity [Expr a]
woo = do
  x <- sym "x"
  let y = abs x
  z <- node (x*y)
  outputs [z, z*y]

run :: Num a => FunGraph a
run = makeFun woo
