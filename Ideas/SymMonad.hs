{-# OPTIONS_GHC -Wall #-}
{-# Language GADTs #-}
{-# Language FlexibleContexts #-}
{-# Language TypeOperators #-}
{-# Language TypeFamilies #-}

module Ideas.SymMonad ( (:*)(..)
                      , HList(..)
                      , node
                      , inputs
                      , outputs
                      , exampleFun
                      , run
                      , makeFun
                      ) where

import Control.Monad.State(State,get,put,liftM,runState)
import Data.Array.Repa(DIM0,DIM1,DIM2,listOfShape,Shape)
import Data.Hashable(Hashable)
import Data.Vector.Unboxed(Unbox)

import Ideas.Graph
import Ideas.Expr

-- | take all sub expressions of an Expr and turn them into nodes
--   return an Expr that is just a ref
node :: (Shape d, Hashable a, Unbox a, Eq a) => Expr d a -> State (FunGraph a b c) (Expr d a)
node expr = liftM (ERef (dim expr)) (node' expr)
  where
    node' :: (Shape d, Hashable a, Unbox a, Eq a) => Expr d a -> State (FunGraph a b c) Key
    node' (ESym d name) = insert $ GSym (listOfShape d) name
    node' (ERef _ k) = return k
    node' (EBinary op x y) = do
      x' <- node' x
      y' <- node' y
      insert (GBinary  op x' y')
    node' (EUnary op x) = do
      x' <- node' x
      insert (GUnary op x')
    node' (EConst d x) = insert $ GConst (listOfShape d) x
    node' (EDimensionless _) = error "don't put EDimensionless in graph, ya goon"
    node' (ESingleton d x) = insert $ GSingleton (listOfShape d) x
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

---------------------- heterogenous inputs/outputs ------------------
data a :* b = a :* b deriving Show
infixr 6 :*

class HList a where
  type NumT a
  type DimT a
  mkNodes :: (NumT a ~ b) => a -> State (FunGraph b c d) (a,[Key])
  getHDim :: a -> DimT a

instance (HList a, HList b, NumT a ~ NumT b) => HList (a :* b) where
  type NumT (a :* b) = NumT a
  type DimT (a :* b) = (DimT a) :* (DimT b)
  mkNodes (x :* y) = do
    (exs,kxs) <- mkNodes x
    (eys,kys) <- mkNodes y
    return (exs :* eys, kxs++kys)
  getHDim (x :* y) = (getHDim x) :* (getHDim y)

instance (Shape d, Hashable a, Unbox a, Eq a) => HList (Expr d a) where
  type NumT (Expr d a) = a
  type DimT (Expr d a) = d
  mkNodes expr = do
    expr'@(ERef _ k) <- node expr
    return (expr', [k])
  getHDim = dim

inputs :: HList b => b -> State (FunGraph (NumT b) (DimT b) c) b
inputs exprs = do
  (exprs', keys) <- mkNodes exprs
  FunGraph xs _ outs <- get
  put (FunGraph xs (getHDim exprs, keys) outs)
  return exprs'

outputs :: HList c => c -> State (FunGraph (NumT c) b (DimT c)) c
outputs exprs = do
  (exprs',keys) <- mkNodes exprs
  FunGraph xs ins _ <- get
  put (FunGraph xs ins (getHDim exprs,keys))
  return exprs'

inputs_ :: HList b => b -> State (FunGraph (NumT b) (DimT b) c) ()
inputs_ exprs = do
  _ <- inputs exprs
  return ()

outputs_ :: HList c => c -> State (FunGraph (NumT c) b (DimT c)) ()
outputs_ exprs = do
  _ <- outputs exprs
  return ()


---------------- utility functions -----------------
makeFun :: State (FunGraph a b c) d -> (d, FunGraph a b c)
makeFun f = runState f emptyFunGraph

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
  print gr
  putStrLn $ showCollisions gr
  previewGraph gr
