{-# OPTIONS_GHC -Wall #-}
{-# Language FlexibleContexts #-}

module Numeric.Dvda.Symbolic.SymMonad( makeFun
                                     , sym
                                     , outputs
                                     , node
                                     , woo
                                     ) where

import Control.Monad.State
import Data.Functor.Identity

import Numeric.Dvda.Symbolic.BinUn

type Key = Int

data Graph = Graph [(Key, Expr)] deriving (Show, Eq)

data Expr = Sym String
          | Binary BinOp Expr Expr
          | Unary UnOp Expr
          | ConstI Int
          | Output Expr
          | Ref Key deriving (Show, Eq)

instance Num Expr where
  (*) = Binary Mul
  (+) = Binary Add
  (-) = Binary Sub
  abs = Unary Abs
  negate = Unary Neg
  signum = Unary Signum
  fromInteger = ConstI . fromInteger

freeKey :: Graph -> Key
freeKey (Graph xs) = length xs

insert :: Key -> Expr -> Graph -> Graph
insert k el (Graph xs) = Graph $ xs ++ [(k, el)]

sym :: String -> State Graph Expr
sym = node . Sym

--outputs :: MonadState Graph m => [Expr] -> m [Expr]
outputs :: [Expr] -> StateT Graph Identity [Expr]
outputs = mapM (node . Output)

--node :: MonadState Graph m => Expr -> m Expr
node :: Expr -> State Graph Expr
node expr = do
  gr <- get
  let k = freeKey gr
  put (insert k expr gr)
  return (Ref k)

makeFun :: State Graph a -> Graph
makeFun f = snd $ runState f (Graph [])

woo :: StateT Graph Identity [Expr]
woo = do
  x <- sym "x"
  let y = abs x
  z <- node (x*y)
  outputs [z, z*y]
