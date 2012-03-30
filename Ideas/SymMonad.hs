{-# OPTIONS_GHC -Wall #-}
{-# Language GADTs #-}
{-# Language FlexibleContexts #-}

module Ideas.SymMonad( sym
                     , symVec
                     , symMat
                     , node
                     , FunGraph(..)
                     , woo
                     , run
                     , makeFun
                     ) where

import Control.Monad.State
import Data.Functor.Identity
import Data.Array.Repa(DIM0,DIM1,DIM2,listOfShape,Shape)
import Data.Vector.Unboxed(Vector)

import Ideas.StrongExpr
import Ideas.BinUn(BinOp, UnOp)

type Key = Int

data GExpr a = GBinary BinOp Key Key
             | GUnary UnOp Key
             | GSym String
             | GScale Key Key
             | GDot Key Key
             | GDeriv Key Key
             | GGrad Key Key
             | GJacob Key Key
             | GConst [Int] (Vector a) deriving (Show, Eq)
                                 
data FunGraph a = FunGraph [(Key, GExpr a)] [Key] [Key] deriving (Show, Eq)

sym :: String -> State (FunGraph a) (Expr DIM0 a)
sym = node . symE

symVec :: Int -> String -> State (FunGraph a) (Expr DIM1 a)
symVec d = node . (vsymE d)

symMat :: (Int,Int) -> String -> State (FunGraph a) (Expr DIM2 a)
symMat (r,c) = node . (msymE (r,c))

-- | take all sub expressions of an Expr and turn them into nodes
--   return an Expr that is just a ref
node :: Shape d => Expr d a -> State (FunGraph a) (Expr d a)
node expr = liftM (ERef (dim expr)) (node' expr)
  where
    --node' :: Shape d => Expr d a -> StateT (FunGraph a) Identity Int
    node' :: Shape d => Expr d a -> State (FunGraph a) Int
    node' (ESym _ name) = insert $ GSym name
    node' (ERef _ k) = return k
    node' (EBinary op x y) = do
      x' <- node' x
      y' <- node' y
      insert (GBinary  op x' y')
    node' (EUnary op x) = do
      x' <- node' x
      insert (GUnary op x')
    node' (EConst d x) = insert $ GConst (listOfShape d) x
    node' (ESingleton _) = error "don't put ESingleton in graph"
    node' (EScale x y) = do
      x' <- node' x
      y' <- node' y
      insert $ GScale x' y'
    node' (EDot x y) = do
      x' <- node' x
      y' <- node' y
      insert $ GDot x' y'
    node' (EDeriv x args) = do
      x' <- node' x
      args' <- node' args
      insert $ GDeriv x' args'
    node' (EGrad x args) = do
      x' <- node' x
      args' <- node' args
      insert $ GGrad x' args'
    node' (EJacob x args) = do
      x' <- node' x
      args' <- node' args
      insert $ GJacob x' args'

    -- insert :: MonadState (FunGraph a) m => GExpr a -> m Int
    insert gexpr = do
      FunGraph xs ins outs <- get
      let k = length xs
      put (FunGraph (xs ++ [(k,gexpr)]) ins outs)
      return k


makeFun :: State (FunGraph a) b -> (b, FunGraph a)
makeFun f = runState f (FunGraph [] [] [])

--woo :: Num a => StateT (FunGraph a) Identity [Expr d a]
woo :: Num a => StateT (FunGraph a) Identity [Expr DIM0 a]
woo = do
  x <- sym "x"
  let y = abs x
  z <- node (x*y)
  return [z, z*y]

run :: Num b => ([Expr DIM0 b], FunGraph b)
run = makeFun woo
