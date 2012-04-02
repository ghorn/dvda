{-# OPTIONS_GHC -Wall #-}
{-# Language GADTs #-}
{-# Language FlexibleContexts #-}

module Ideas.SymMonad( sym
                     , symVec
                     , symMat
                     , node
                     , output
                     , output_
                     , exampleFun
                     , run
                     , makeFun
                     ) where

import Control.Monad.State
import Data.Array.Repa(DIM0,DIM1,DIM2,listOfShape,Shape)
import Data.Hashable(Hashable)
import Data.Vector.Unboxed(Unbox)

import Ideas.Graph(FunGraph(..),Key,GExpr(..),previewGraph,insert,emptyFunGraph)
import Ideas.Expr

sym :: (Eq a, Hashable a, Unbox a) => String -> State (FunGraph a) (Expr DIM0 a)
sym = node . symE

symVec :: (Eq a, Hashable a, Unbox a) => Int -> String -> State (FunGraph a) (Expr DIM1 a)
symVec d = node . vsymE d

symMat :: (Eq a, Hashable a, Unbox a) => (Int,Int) -> String -> State (FunGraph a) (Expr DIM2 a)
symMat (r,c) = node . msymE (r,c)

-- | take all sub expressions of an Expr and turn them into nodes
--   return an Expr that is just a ref
node :: (Shape d, Hashable a, Unbox a, Eq a) => Expr d a -> State (FunGraph a) (Expr d a)
node expr = liftM (ERef (dim expr)) (node' expr)
  where
    --node' :: (Shape d, Hashable a, Unbox a, Eq a) => Expr d a -> StateT (FunGraph a) Identity Int
    node' :: (Shape d, Hashable a, Unbox a, Eq a) => Expr d a -> State (FunGraph a) Key
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

output :: (Eq a, Hashable a, Unbox a, Shape d) => Expr d a -> State (FunGraph a) (Expr d a)
output expr = do
  eref@(ERef _ k) <- node expr
  FunGraph xs ins outs <- get
  put (FunGraph xs ins (outs ++ [k]))
  return eref
  
output_ :: (Eq a, Hashable a, Unbox a, Shape d) => Expr d a -> State (FunGraph a) ()
output_ expr = do
  _ <- output expr
  return ()


makeFun :: State (FunGraph a) b -> (b, FunGraph a)
makeFun f = runState f emptyFunGraph

exampleFun :: State (FunGraph Double) [Expr DIM0 Double]
exampleFun = do
  x <- sym "x"
  y <- sym "y"
  z1 <- node ((x*y)**3)
  z2 <- node ((x*y)**2)
  z3 <- node (diff z2 x)
--  output_ (z1*z3)
  return [z1, z2*y]

run :: IO ()
run = do
  let gr :: FunGraph Double
      gr = snd $ makeFun exampleFun
  print gr
  previewGraph gr
