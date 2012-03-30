{-# OPTIONS_GHC -Wall #-}
{-# Language GADTs #-}
{-# Language FlexibleContexts #-}

module Ideas.SymMonad( sym
                     , symVec
                     , symMat
                     , node
                     , exampleFun
                     , run
                     , makeFun
                     ) where

import Control.Monad.State
import Data.Array.Repa(DIM0,DIM1,DIM2,listOfShape,Shape)
import qualified Data.IntMap as IM(null,insert,empty,findMax)

import Ideas.Graph(FunGraph(..),Key,GExpr(..),previewGraph)
import Ideas.StrongExpr

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
    node' :: Shape d => Expr d a -> State (FunGraph a) Key
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

    -- insert :: MonadState (FunGraph a) m => GExpr a -> m Int
    insert gexpr = do
      FunGraph xs ins outs <- get
      let k = if IM.null xs then 0
              else 1 + (fst $ IM.findMax xs)
          ins' = case gexpr of (GSym _ _) -> ins++[k] -- add Sym to FunGraph inputs
                               _          -> ins
      put (FunGraph (IM.insert k gexpr xs) ins' outs)
      return k


makeFun :: State (FunGraph a) b -> (b, FunGraph a)
makeFun f = runState f (FunGraph IM.empty [] [])

exampleFun :: Floating a => State (FunGraph a) [Expr DIM0 a]
exampleFun = do
  x <- sym "x"
  let y = abs x
  z <- node (x*y**3)
  return [z, z*y]

run :: IO ()
run = do
  let gr :: FunGraph Double
      gr = snd $ makeFun exampleFun
  print gr
  previewGraph gr
