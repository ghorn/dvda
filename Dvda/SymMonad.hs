{-# OPTIONS_GHC -Wall #-}
{-# Language GADTs #-}
{-# Language FlexibleContexts #-}
{-# Language TypeOperators #-}
{-# Language TypeFamilies #-}

module Dvda.SymMonad ( (:*)(..)
                     , HList(..)
                     , node
                     , inputs
                     , inputs_
                     , outputs
                     , outputs_
                     , makeFun
                     ) where
  
import Control.Monad ( when )
import Control.Monad.State ( MonadState, State, get, put, liftM, runState )
import Data.Array.Repa ( listOfShape, Shape )
import Data.Hashable ( Hashable )
import Data.Vector.Unboxed ( Unbox )
import Data.Maybe ( fromJust )
import qualified Data.HashMap.Strict as HM
import qualified Data.IntMap as IM

import Dvda.Graph ( FunGraph(..), GExpr(..), DerivMap, Key, emptyFunGraph )
import Dvda.Expr ( Expr(..), dim, scale, dot )
import Dvda.BinUn ( binaryDeriv, unaryDeriv )

-- | take all sub expressions of an Expr and turn them into nodes
--   return an Expr that is just a ref
node :: (Shape d, Hashable a, Unbox a, Floating a, Eq a) => Expr d a -> State (FunGraph a b c) (Expr d a)
node expr = liftM (ERef (dim expr)) (node' expr)
  where
    node' :: (Shape d, Hashable a, Unbox a, Floating a, Eq a) => Expr d a -> State (FunGraph a b c) Key
    node' (ERef _ k) = return k
    node' (ESym d name) = do
      let gexpr = GSym (listOfShape d) name
          mkDerivMap = do pert <- node' (ESingleton d 1)
                          return (HM.singleton gexpr pert)
      insert gexpr mkDerivMap
    node' (EBinary op x_ y_) = do
      xk <- node' x_
      yk <- node' y_
      (FunGraph _ im _ _) <- get
      let d = dim x_
          x = ERef d xk
          y = ERef d yk
          xperts = snd $ fromJust (IM.lookup xk im)
          yperts = snd $ fromJust (IM.lookup yk im)
          -- vars is all the variables which the node is a function of
          -- switch to HM.keysSet when it is created
          vars = HM.keys (HM.union xperts yperts)
          mkDerivMap = mapM diffBinary vars >>= (return . HM.fromList)
            where
              diffBinary var = do 
                let x' = case HM.lookup var xperts of Nothing -> ESingleton d 0
                                                      Just k  -> ERef d k
                    y' = case HM.lookup var yperts of Nothing -> ESingleton d 0
                                                      Just k  -> ERef d k
                pert <- node' (binaryDeriv op (x,x') (y,y'))
                return (var, pert)
      insert (GBinary op xk yk) mkDerivMap
    node' (EUnary op x) = do
      x' <- node' x
      (FunGraph _ im _ _) <- get
      let d = dim x
          mkDerivMap = mapM diffUnary (HM.toList (snd $ fromJust $ IM.lookup x' im))
                       >>= (return . HM.fromList)
            where
              diffUnary (var,childPert) = do pert <- node' (unaryDeriv op (ERef d x', ERef d childPert))
                                             return (var, pert)
      insert (GUnary op x') mkDerivMap
    node' (EConst d x) = do
      let d' = listOfShape d
          gexpr = GConst d' x
          mkDerivMap = do pert <- node' (ESingleton d 0)
                          return (HM.singleton gexpr pert)
      insert gexpr mkDerivMap
    node' (EDimensionless _) = error "don't put EDimensionless in graph, ya goon"
    node' (ESingleton d x) = do
      let gexpr = GSingleton (listOfShape d) x
          mkDerivMap = do pert <- node' (ESingleton d 0)
                          return (HM.singleton gexpr pert)
      insert gexpr mkDerivMap
    node' (EScale x_ y_) = do
      xk <- node' x_
      yk <- node' y_
      (FunGraph _ im _ _) <- get
      let dx = dim x_
          dy = dim y_
          x = ERef dx xk
          y = ERef dy yk
          xperts = snd $ fromJust (IM.lookup xk im)
          yperts = snd $ fromJust (IM.lookup yk im)
          -- vars is all the variables which the node is a function of
          -- switch to HM.keysSet when it is created
          vars = HM.keys (HM.union xperts yperts)
          mkDerivMap = mapM diffScale vars >>= (return . HM.fromList)
            where
              diffScale var = do 
                let x' = case HM.lookup var xperts of Nothing -> ESingleton dx 0
                                                      Just k  -> ERef dx k
                    y' = case HM.lookup var yperts of Nothing -> ESingleton dy 0
                                                      Just k  -> ERef dy k
                pert <- node' (scale x y' + scale x' y)
                return (var, pert)
      insert (GScale xk yk) mkDerivMap
      
    node' (EDot x_ y_) = do
      xk <- node' x_
      yk <- node' y_
      (FunGraph _ im _ _) <- get
      let dx = dim x_
          dy = dim y_
          x = ERef dx xk
          y = ERef dy yk
          xperts = snd $ fromJust (IM.lookup xk im)
          yperts = snd $ fromJust (IM.lookup yk im)
          -- vars is all the variables which the node is a function of
          -- switch to HM.keysSet when it is created
          vars = HM.keys (HM.union xperts yperts)
          mkDerivMap = mapM diffDot vars >>= (return . HM.fromList)
            where
              diffDot var = do
                let x' = case HM.lookup var xperts of Nothing -> ESingleton dx 0
                                                      Just k  -> ERef dx k
                    y' = case HM.lookup var yperts of Nothing -> ESingleton dy 0
                                                      Just k  -> ERef dy k
                pert <- node' (dot x y' + dot x' y)
                return (var, pert)
      insert (GDot xk yk) mkDerivMap

    node' (EDeriv x arg) = do
      x' <- node' x
      arg' <- node' arg
      (FunGraph _ im _ _) <- get
      let var      = fst $ fromJust $ IM.lookup arg' im
          derivMap = snd $ fromJust $ IM.lookup x' im
          isSym (GSym _ _) = True
          isSym _ = False
      when (not (isSym var)) $ error $ "can't take a derivative of something that isn't symbolic"
      case HM.lookup var derivMap of Nothing     -> node' $ ESingleton (dim arg) 0
                                     Just derivK -> return derivK

    node' (EGrad x args) = do
      x' <- node' x
      args' <- node' args
      (FunGraph _ im _ _) <- get
      let var      = fst $ fromJust $ IM.lookup args' im
          derivMap = snd $ fromJust $ IM.lookup x' im
          isSym (GSym _ _) = True
          isSym _ = False
      when (not (isSym var)) $ error "can't take a derivative of something that isn't symbolic"
      case HM.lookup var derivMap of Nothing     -> node' $ ESingleton (dim args) 0
                                     Just derivK -> return derivK

    node' (EJacob xs args) = do
      xs' <- node' xs
      args' <- node' args
      (FunGraph _ im _ _) <- get
      let var      = fst $ fromJust $ IM.lookup args' im
          derivMap = snd $ fromJust $ IM.lookup xs' im
          isSym (GSym _ _) = True
          isSym _ = False
      when (not (isSym var)) $ error "can't take a derivative of something that isn't symbolic"
      case HM.lookup var derivMap of Nothing     -> node' $ ESingleton (dim args) 0
                                     Just derivK -> return derivK


-- | Try to insert the GExpr into the hashmap performing CSE.
--   Take a GExpr and it's perturbation GExprs.
--   If the GExpr is not yet in the map, insert it and return new key.
--   Otherwise don't insert, just return existing key.
insert :: (Eq a, Hashable a, Unbox a, MonadState (FunGraph a b c) m) => GExpr a -> m (DerivMap a) -> m Key
insert gexpr mkDerivMap = do
  (FunGraph hm im ins outs) <- get
  case HM.lookup gexpr hm of Just k' -> return k'
                             Nothing -> do derivMap <- mkDerivMap
                                           let k = HM.size hm
                                               hm' = HM.insert gexpr k hm
                                               im' = IM.insert k (gexpr,derivMap) im
                                           put $ FunGraph hm' im' ins outs
                                           return k
                             

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

instance (Shape d, Hashable a, Unbox a, Eq a, Floating a) => HList (Expr d a) where
  type NumT (Expr d a) = a
  type DimT (Expr d a) = d
  mkNodes expr = do
    expr'@(ERef _ k) <- node expr
    return (expr', [k])
  getHDim = dim
  
inputs :: HList b => b -> State (FunGraph (NumT b) (DimT b) c) b
inputs exprs = do
  (exprs', keys) <- mkNodes exprs
  FunGraph hm im _ outs <- get
  put (FunGraph hm im (getHDim exprs, keys) outs)
  return exprs'

outputs :: HList c => c -> State (FunGraph (NumT c) b (DimT c)) c
outputs exprs = do
  (exprs',keys) <- mkNodes exprs
  FunGraph hm im ins _ <- get
  put (FunGraph hm im ins (getHDim exprs,keys))
  return exprs'

inputs_ :: HList b => b -> State (FunGraph (NumT b) (DimT b) c) ()
inputs_ exprs = do
  _ <- inputs exprs
  return ()

outputs_ :: HList c => c -> State (FunGraph (NumT c) b (DimT c)) ()
outputs_ exprs = do
  _ <- outputs exprs
  return ()

---------------- utility function -----------------
makeFun :: State (FunGraph a b c) d -> (d, FunGraph a b c)
makeFun f = runState f emptyFunGraph
