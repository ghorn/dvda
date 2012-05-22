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

import Control.Monad.State.Lazy ( StateT )
import Data.Functor.Identity ( Identity )

import Control.Monad.State ( MonadState, State, get, put, liftM, runState )
import Data.Array.Repa ( listOfShape, Shape )
import Data.Hashable ( Hashable )
import Data.Vector.Unboxed ( Unbox )
import Data.Maybe ( fromJust )
import qualified Data.HashMap.Strict as HM
import qualified Data.IntMap as IM

import Dvda.Graph ( FunGraph(..), GExpr(..), LazyDeriv(..), DerivMap, Key, emptyFunGraph, fgLookup, fgReverseLookup, fgGExprFromKey )
import Dvda.Expr ( Expr(..), dim, scale, dot )
import Dvda.BinUn ( binaryDeriv, unaryDeriv )

-- __VERY INEFFICIENT__
-- replace once mapWithKey function is added to unordered-containers
hashmapMapWithKey :: (Eq k, Hashable k) => (k -> a -> b) -> HM.HashMap k a -> HM.HashMap k b
hashmapMapWithKey f hm = HM.fromList $ map g (HM.toList hm)
  where
    g (x,y) = (x, f x y)

-- evaluates the derivative and inserts the Evaluated form back into the graph
evalLazyDeriv :: (Eq a, Hashable a, Unbox a) => GExpr a -> GExpr a -> StateT (FunGraph a b c) Identity (Maybe Key)
evalLazyDeriv expr arg = do
  -- lookup the function which makes the derivative
  fg0 <- get
  let (_, derivMap0) = fromJust $ fgLookup expr fg0
      lazyMkDeriv = HM.lookup arg derivMap0
--        Nothing -> error "some goon tried to evalLazyDeriv when there was no value"
--        Just lazyMkDeriv_ -> lazyMkDeriv_
        
  case lazyMkDeriv of
    Nothing                      -> return Nothing
    -- return the derivative if it has already been computed
    (Just (Evaluated derivK))    -> return (Just derivK)
    -- otherwise compute the derivative and insert evaluated form into graph
    (Just (Unevaluated mkDeriv)) -> do
      derivK <- mkDeriv
      fg1@(FunGraph hm1 im1 ins outs) <- get
      let (k, derivMap1) = fromJust $ fgLookup expr fg1
          derivMap2 = HM.insert arg (Evaluated derivK) derivMap1
          hm2 = HM.insert expr (k, derivMap2) hm1
      put (FunGraph hm2 im1 ins outs)
      return (Just derivK)


-- | take all sub expressions of an Expr and turn them into nodes
--   return an Expr that is just a ref
node :: (Shape d, Hashable a, Unbox a, Floating a, Eq a) => Expr d a -> StateT (FunGraph a b c) Identity (Expr d a)
node expr = liftM (ERef (dim expr)) (node' expr)
  where
    node' :: (Shape d, Hashable a, Unbox a, Floating a, Eq a) => Expr d a -> StateT (FunGraph a b c) Identity Key
    node' (EDimensionless _) = error "don't put EDimensionless in graph, ya goon"
    node' (ERef _ k) = return k
    node' (ESym d name) = do
      let gexpr = GSym (listOfShape d) name
          derivMap = HM.singleton gexpr (Unevaluated $ node' (ESingleton d 1))
      insert gexpr derivMap
    node' (EConst d x) = do
      let d' = listOfShape d
          gexpr = GConst d' x
      insert gexpr HM.empty
    node' (ESingleton d x) = do
      let gexpr = GSingleton (listOfShape d) x
      insert gexpr HM.empty

    node' (EUnary op x) = do
      -- insert the child node
      x' <- node' x
      fg <- get
      let d = dim x
          childPrimal = ERef d x'
          childGExpr = fromJust $ fgGExprFromKey x' fg
          (_, childDerivs) = fromJust $ fgReverseLookup x' fg
          derivMap = hashmapMapWithKey mkDeriv childDerivs
            where
              mkDeriv arg _ = Unevaluated $ do
                childPert <- evalLazyDeriv childGExpr arg
                node' $ unaryDeriv op (childPrimal, ERef d (fromJust childPert))
      -- insert the parent node
      insert (GUnary op x') derivMap
    
    node' (EBinary op x_ y_) = do
      xk <- node' x_
      yk <- node' y_
      fg <- get
      let d = dim x_
          x = ERef d xk
          y = ERef d yk
          xgexpr = fromJust $ fgGExprFromKey xk fg
          ygexpr = fromJust $ fgGExprFromKey yk fg
          -- vars is all the variables which the node is a function of
          -- switch to HM.keysSet when it is created
          vars = HM.keys (HM.union xperts yperts)
            where
              xperts = snd $ fromJust (fgReverseLookup xk fg)
              yperts = snd $ fromJust (fgReverseLookup yk fg)
          derivMap = HM.fromList $ map diffBinary vars
            where
              diffBinary arg = (arg, diffBinary' arg)
              diffBinary' arg = Unevaluated $ do
                xk_ <- evalLazyDeriv xgexpr arg
                yk_ <- evalLazyDeriv ygexpr arg
                
                let x' = case xk_ of Nothing -> ESingleton d 0
                                     Just k  -> ERef d k
                    y' = case yk_ of Nothing -> ESingleton d 0
                                     Just k  -> ERef d k
                node' (binaryDeriv op (x,x') (y,y'))
      insert (GBinary op xk yk) derivMap

    node' (EScale x_ y_) = do
      xk <- node' x_
      yk <- node' y_
      fg <- get
      let d = dim x_
          x = ERef d xk
          y = ERef d yk
          xgexpr = fromJust $ fgGExprFromKey xk fg
          ygexpr = fromJust $ fgGExprFromKey yk fg
          -- vars is all the variables which the node is a function of
          -- switch to HM.keysSet when it is created
          vars = HM.keys (HM.union xperts yperts)
            where
              xperts = snd $ fromJust (fgReverseLookup xk fg)
              yperts = snd $ fromJust (fgReverseLookup yk fg)
          derivMap = HM.fromList $ map diffBinary vars
            where
              diffBinary arg = (arg, diffBinary' arg)
              diffBinary' arg = Unevaluated $ do
                xk_ <- evalLazyDeriv xgexpr arg
                yk_ <- evalLazyDeriv ygexpr arg
                
                let x' = case xk_ of Nothing -> ESingleton d 0
                                     Just k  -> ERef d k
                    y' = case yk_ of Nothing -> ESingleton d 0
                                     Just k  -> ERef d k
                node' (scale x y' + scale x' y)
      insert (GScale xk yk) derivMap

    node' (EDot x_ y_) = do
      xk <- node' x_
      yk <- node' y_
      fg <- get
      let dx = dim x_
          dy = dim y_
          x = ERef dx xk
          y = ERef dy yk
          xgexpr = fromJust $ fgGExprFromKey xk fg
          ygexpr = fromJust $ fgGExprFromKey yk fg
          -- vars is all the variables which the node is a function of
          -- switch to HM.keysSet when it is created
          vars = HM.keys (HM.union xperts yperts)
            where
              xperts = snd $ fromJust (fgReverseLookup xk fg)
              yperts = snd $ fromJust (fgReverseLookup yk fg)
          derivMap = HM.fromList $ map diffBinary vars
            where
              diffBinary arg = (arg, diffBinary' arg)
              diffBinary' arg = Unevaluated $ do
                xk_ <- evalLazyDeriv xgexpr arg
                yk_ <- evalLazyDeriv ygexpr arg
                
                let x' = case xk_ of Nothing -> ESingleton dx 0
                                     Just k  -> ERef dx k
                    y' = case yk_ of Nothing -> ESingleton dy 0
                                     Just k  -> ERef dy k
                node' (dot x y' + dot x' y)
      insert (GDot xk yk) derivMap

    node' (EDeriv x arg) = do
      x' <- node' x
      arg' <- node' arg
      fg <- get
      let xG   = fromJust $ fgGExprFromKey x' fg
          argG = fromJust $ fgGExprFromKey arg' fg
      derivK <- evalLazyDeriv xG argG
      case derivK of Nothing -> node' $ ESingleton (dim arg) 0
                     Just k  -> return k

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
--   Take a GExpr and it's perturbation GExprs.
--   If the GExpr is not yet in the map, insert it and return new key.
--   Otherwise don't insert, just return existing key.
--insert :: (Eq a, Hashable a, Unbox a, MonadState (FunGraph a b c) m) => GExpr a -> (DerivMap a b c) -> m Key
insert :: (Hashable a, Unbox a, Floating a, Eq a) =>
          GExpr a -> DerivMap a b c -> StateT (FunGraph a b c) Identity Key
insert gexpr derivMap = do
  (FunGraph hm im ins outs) <- get
  case HM.lookup gexpr hm of
    Just (k',_) -> return k'
    Nothing -> do let k = HM.size hm
                      hm' = HM.insert gexpr (k,derivMap) hm
                      im' = IM.insert k gexpr im
                  put (FunGraph hm' im' ins outs)
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
