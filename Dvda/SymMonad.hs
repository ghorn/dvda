{-# OPTIONS_GHC -Wall #-}
{-# Language GADTs #-}
{-# Language FlexibleContexts #-}
{-# Language TypeOperators #-}
{-# Language TypeFamilies #-}

module Dvda.SymMonad ( (:*)(..)
                     , HList(..)
                     , node
                     , node'
                     , inputs
                     , inputs_
                     , outputs
                     , outputs_
                     , makeFun
                     ) where

import Control.Monad.State.Lazy ( StateT )
import Data.Functor.Identity ( Identity )

import Control.Monad.State ( MonadState, State, get, put, liftM, runState )
import Data.Array.Repa ( listOfShape, Shape )
import Data.Hashable ( Hashable )
import Data.Vector.Unboxed ( Unbox )
import Data.Maybe ( fromJust )
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
import qualified Data.IntMap as IM

import Dvda.Graph ( FunGraph(..), GExpr(..), SymSet, Key, emptyFunGraph, fgReverseLookup )
import Dvda.Expr ( Expr(..), dim )

-- | take all sub expressions of an Expr and turn them into nodes
--   return an Expr that is just a ref
node :: (Shape sh, Hashable a, Unbox a, Floating a, Eq a) => Expr sh a -> StateT (FunGraph a b c) Identity (Expr sh a)
node expr = liftM (ERef (dim expr)) (node' expr)

node' :: (Shape sh, Hashable a, Unbox a, Floating a, Eq a) => Expr sh a -> StateT (FunGraph a b c) Identity Key
node' (EDimensionless _) = error "don't put EDimensionless in graph, ya goon"
node' (ERef _ k) = return k
node' (ESym sh name) = do
  let gexpr = GSym (listOfShape sh) name
  insert gexpr (HS.singleton gexpr)
node' (EConst sh x) = do
  let gexpr = GConst (listOfShape sh) x
  insert gexpr HS.empty
node' (ESingleton sh x) = do
  let gexpr = GSingleton (listOfShape sh) x
  insert gexpr HS.empty
node' (EUnary op x) = do
  xk <- node' x
  fg <- get
  let (_,symMap) = fromJust $ fgReverseLookup xk fg
  insert (GUnary op xk) symMap
node' (EBinary op x y) = do
  xk <- node' x
  yk <- node' y
  fg <- get
  let (_,symMapX) = fromJust $ fgReverseLookup xk fg
      (_,symMapY) = fromJust $ fgReverseLookup yk fg
  insert (GBinary op xk yk) (HS.union symMapX symMapY)
node' (EScale x y) = do
  xk <- node' x
  yk <- node' y
  fg <- get
  let (_,symMapX) = fromJust $ fgReverseLookup xk fg
      (_,symMapY) = fromJust $ fgReverseLookup yk fg
  insert (GScale xk yk) (HS.union symMapX symMapY)
node' (EDot x y) = do
  xk <- node' x
  yk <- node' y
  fg <- get
  let (_,symMapX) = fromJust $ fgReverseLookup xk fg
      (_,symMapY) = fromJust $ fgReverseLookup yk fg
  insert (GDot xk yk) (HS.union symMapX symMapY)
--    node' (EDeriv x arg) = do
--      xk <- node' x
--      argk <- node' arg
--      fg <- get
--      let xG   = fromJust $ fgGExprFromKey xk fg
--          argG = fromJust $ fgGExprFromKey argk fg
--      derivK <- evalLazyDeriv xG argG
--      case derivK of Nothing -> node' $ ESingleton (dim arg) 0
--                     Just k  -> return k
--    node' (EGrad x args) = do
--      x' <- node' x
--      args' <- node' args
--      (FunGraph _ im _ _) <- get
--      let var      = fst $ fromJust $ IM.lookup args' im
--          derivMap = snd $ fromJust $ IM.lookup x' im
--          isSym (GSym _ _) = True
--          isSym _ = False
--      when (not (isSym var)) $ error "can't take a derivative of something that isn't symbolic"
--      case HM.lookup var derivMap of Nothing     -> node' $ ESingleton (dim args) 0
--                                     Just derivK -> return derivK
--
--    node' (EJacob xs args) = do
--      xs' <- node' xs
--      args' <- node' args
--      (FunGraph _ im _ _) <- get
--      let var      = fst $ fromJust $ IM.lookup args' im
--          derivMap = snd $ fromJust $ IM.lookup xs' im
--          isSym (GSym _ _) = True
--          isSym _ = False
--      when (not (isSym var)) $ error "can't take a derivative of something that isn't symbolic"
--      case HM.lookup var derivMap of Nothing     -> node' $ ESingleton (dim args) 0
--                                     Just derivK -> return derivK


-- | Try to insert the GExpr into the hashmap performing CSE.
--   If the GExpr is not yet in the map, insert it and return new key.
--   Otherwise don't insert, just return existing key.
--insert :: (Eq a, Hashable a, Unbox a, MonadState (FunGraph a b c) m) => GExpr a -> (DerivMap a b c) -> m Key
insert :: (Hashable a, Unbox a, Floating a, Eq a) =>
          GExpr a -> SymSet a -> StateT (FunGraph a b c) Identity Key
insert gexpr symSet = do
  (FunGraph hm im ins outs) <- get
  case HM.lookup gexpr hm of
    Just (k',_) -> return k'
    Nothing -> do let k = HM.size hm
                      hm' = HM.insert gexpr (k,symSet) hm
                      im' = IM.insert k gexpr im
                  put (FunGraph hm' im' ins outs)
                  return k
                             

---------------------- heterogenous inputs/outputs ------------------
data a :* b = a :* b deriving Show
infixr 6 :*

class HList a where
  type NumT a
  type DimT a
--  mkNodes :: (NumT a ~ b) => a -> State (FunGraph b c d) (a,[Key])
  mkNodes :: a -> State (FunGraph (NumT a) b c) (a,[Key])
  getHDim :: a -> DimT a

instance (HList a, HList b, NumT a ~ NumT b) => HList (a :* b) where
  type NumT (a :* b) = NumT a
  type DimT (a :* b) = (DimT a) :* (DimT b)
  mkNodes (x :* y) = do
    (exs,kxs) <- mkNodes x
    (eys,kys) <- mkNodes y
    return (exs :* eys, kxs++kys)
  getHDim (x :* y) = (getHDim x) :* (getHDim y)

instance (Shape sh, Hashable a, Unbox a, Eq a, Floating a) => HList (Expr sh a) where
  type NumT (Expr sh a) = a
  type DimT (Expr sh a) = sh
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
