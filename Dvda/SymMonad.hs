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
                     , rad
                     , getSensitivities
                     ) where

import Control.Monad ( foldM )
import Control.Monad.State ( MonadState, StateT, get, put, liftM, runState )
import Data.Functor.Identity ( Identity )
import Data.Array.Repa ( Shape )
import Data.Hashable ( Hashable )
import Data.Vector.Unboxed ( Unbox )
import Data.Maybe ( fromJust )

import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
import qualified Data.IntMap as IM
import Data.IntMap ( Key )

import Dvda.Dual
import Dvda.BinUn

import Dvda.Graph ( FunGraph(..), emptyFunGraph, fgReverseLookup, fgGExprFromKey, exprOfGExpr )
import Dvda.GExpr ( GExpr(..), gdim )
import Dvda.Expr ( Expr(..), Dot(..), dim )
import Dvda.HomoDim

-- | take all sub expressions of an Expr and turn them into nodes
--   return an Expr that is just a ref
node :: (Shape sh, Hashable a, Unbox a, Floating a, Eq a) => Expr sh a -> StateT (FunGraph a b c) Identity (Expr sh a)
node expr = liftM (ERef (dim expr)) (node' expr)
            
node' :: (Shape sh, Hashable a, Unbox a, Floating a, Eq a) => Expr sh a -> StateT (FunGraph a b c) Identity Key
node' (EDimensionless _) = error "don't put EDimensionless in graph, ya goon"
node' (ERef _ k) = return k
node' (ESym sh name) = insert $ GSym (homoOfShape sh) name
node' (EConst sh x) = insert $ GConst (homoOfShape sh) x
node' (ESingleton sh x) = insert $ GSingleton (homoOfShape sh) x
node' (EUnary op x) = do
  xk <- node' x
  insert $ GUnary (homoOfShape $ dim x) op xk
node' (EBinary op x y) = do
  xk <- node' x
  yk <- node' y
  insert $ GBinary (homoOfShape $ dim x) op xk yk
node' (EScale x y) = do
  xk <- node' x
  yk <- node' y
  insert $ GScale (homoOfShape $ dim y) xk yk
node' (EDot x y) = do
  xk <- node' x
  yk <- node' y
  let --shx = homoOfShape $ dim x
      --shy = homoOfShape $ dim x
      sh = homoOfShape $ dotDims (dim x) (dim y)
  insert $ GDot sh xk yk
node' (EDeriv x' arg') = do
  x <- node x'
  arg <- node arg'
  outs <- rad x [arg]
  node' (head outs)


-- | Try to insert the GExpr into the hashmap performing CSE.
--   If the GExpr is not yet in the map, insert it and return new key.
--   Otherwise don't insert, just return existing key.
insert :: (Hashable a, Unbox a, Floating a, Eq a) => GExpr a -> StateT (FunGraph a b c) Identity Key
insert gexpr = do
  fg <- get
  let symSet (GSym _ _)          = HS.singleton gexpr
      symSet (GSingleton _ _)    = HS.empty
      symSet (GConst _ _)        = HS.empty
      symSet (GUnary _ _ k)      = snd $ fromJust $ fgReverseLookup k fg
      symSet (GBinary _ _ xk yk) = HS.union symMapX symMapY
        where
          (_,symMapX) = fromJust $ fgReverseLookup xk fg
          (_,symMapY) = fromJust $ fgReverseLookup yk fg
      symSet (GScale _ xk yk) = HS.union symMapX symMapY
        where
          (_,symMapX) = fromJust $ fgReverseLookup xk fg
          (_,symMapY) = fromJust $ fgReverseLookup yk fg
      symSet (GDot _ xk yk) = HS.union symMapX symMapY
        where
          (_,symMapX) = fromJust $ fgReverseLookup xk fg
          (_,symMapY) = fromJust $ fgReverseLookup yk fg

  (FunGraph hm im ins outs) <- get
  case HM.lookup gexpr hm of
    Just (k',_) -> return k'
    Nothing -> do let k = HM.size hm
                      hm' = HM.insert gexpr (k,symSet gexpr) hm
                      im' = IM.insert k gexpr im
                  put (FunGraph hm' im' ins outs)
                  return k


gexprOfExpr :: (Eq a, Floating a, Hashable a, Unbox a, Shape sh) =>
               Expr sh a -> StateT (FunGraph a b c) Identity (GExpr a)
gexprOfExpr expr = do
  k <- node' expr
  fg <- get
  return (fromJust $ fgGExprFromKey k fg)
  
-- gradient of expression w.r.t. list of args
rad :: (Eq a, Hashable a, Unbox a, Floating a, Shape sh) => 
       Expr sh a -> [Expr sh a] -> StateT (FunGraph a b c) Identity [Expr sh a]
rad expr_ args_ = do
  expr <- gexprOfExpr expr_
  args <- mapM gexprOfExpr args_
  let argSet = HS.fromList args
  sensitivities <- getSensitivities argSet expr (ESingleton (dim expr_) 1)
  -- order inputs requested by user
  let orderedSensitivities = map (\x -> fromJust $ HM.lookup x sensitivities) args
      argDims = map dim args_
  return $ zipWith (\sh k -> ERef sh k) argDims orderedSensitivities


-- combine two (GExpr, Key) hashmaps
-- if there is a conflict, add the two GExprs together
unionWithPlus :: (Eq a, Floating a, Hashable a, Unbox a) =>
                 HM.HashMap (GExpr a) Key -> HM.HashMap (GExpr a) Key ->
                 StateT (FunGraph a b c) Identity (HM.HashMap (GExpr a) Key)
unionWithPlus xs ys = foldM addCommon union0 commonGExprs
  where
    -- the gexprs that occur in both maps
    commonGExprs = HM.keys $ HM.intersection xs ys
    -- the initial union that needs conflicts fixed
    union0 = HM.union xs ys
    addCommon hm commonGExpr = do
      let xsensk = fromJust $ HM.lookup commonGExpr xs
          ysensk = fromJust $ HM.lookup commonGExpr ys
      k <- insert $ GBinary (gdim commonGExpr) Add xsensk ysensk
      return (HM.insert commonGExpr k hm)
              

lookupSymSet :: (Eq a, Hashable a, Unbox a) => Key -> StateT (FunGraph a b c) Identity (HS.HashSet (GExpr a))
lookupSymSet k = do
  fg <- get
  let (_,symSet) = fromJust $ fgReverseLookup k fg
  return symSet


getSensitivities :: (Eq a, Hashable a, Unbox a, Floating a, Shape sh) => 
                     HS.HashSet (GExpr a) -> GExpr a -> Expr sh a ->
                     StateT (FunGraph a b c) Identity (HM.HashMap (GExpr a) Key)
getSensitivities _ (GSingleton _ _) _ = return HM.empty
getSensitivities _ (GConst _ _) _ = return HM.empty
getSensitivities args primal@(GSym _ _) sens = case HS.member primal args of
  -- don't backprop if there aren't any interesting symbols farther in the tree
  False -> return HM.empty
  True -> do
    k <- node' sens
    return $ HM.fromList [(primal, k)]
getSensitivities args (GUnary _ op gk) sens = do
  symSetG <- lookupSymSet gk
  case HS.size (HS.intersection args symSetG) of
    -- don't backprop if there aren't any interesting symbols farther in the tree
    0 -> return $ HM.empty
    _ -> do
      fg <- get
      let g' = fromJust $ fgGExprFromKey gk fg
          g = exprOfGExpr g'
          dfdg = dualPerturbation $ applyUnary op (Dual g 1)
      getSensitivities args g' (sens*dfdg)
getSensitivities args (GBinary _ op gk hk) sens = do
  symSetG <- lookupSymSet gk
  symSetH <- lookupSymSet hk
  
  fg <- get
  let g' = fromJust $ fgGExprFromKey gk fg
      h' = fromJust $ fgGExprFromKey hk fg
      g = exprOfGExpr g'
      h = exprOfGExpr h'
      dfdg = dualPerturbation $ applyBinary op (Dual g 1) (Dual h 0)
      dfdh = dualPerturbation $ applyBinary op (Dual g 0) (Dual h 1)
  
  gsens <- case HS.size (HS.intersection args symSetG) of
                0 -> return HM.empty
                _ -> getSensitivities args g' (sens*dfdg)
  hsens <- case HS.size (HS.intersection args symSetH) of
                0 -> return HM.empty
                _ -> getSensitivities args h' (sens*dfdh)
  unionWithPlus gsens hsens
getSensitivities args (GDot _ gk hk) sens = do
  symSetG <- lookupSymSet gk
  symSetH <- lookupSymSet hk
  
  fg <- get
  let g' = fromJust $ fgGExprFromKey gk fg
      h' = fromJust $ fgGExprFromKey hk fg
      g = exprOfGExpr g'
      h = exprOfGExpr h'
      dfdg = h
      dfdh = g
  
  gsens <- case HS.size (HS.intersection args symSetG) of
                0 -> return HM.empty
                _ -> getSensitivities args g' (sens*dfdg)
  hsens <- case HS.size (HS.intersection args symSetH) of
                0 -> return HM.empty
                _ -> getSensitivities args h' (sens*dfdh)
  unionWithPlus gsens hsens
getSensitivities args (GScale _ gk hk) sens = do
  symSetG <- lookupSymSet gk
  symSetH <- lookupSymSet hk
  
  fg <- get
  let g' = fromJust $ fgGExprFromKey gk fg
      h' = fromJust $ fgGExprFromKey hk fg
      g = exprOfGExpr g'
      h = exprOfGExpr h'
      dfdg = h
      dfdh = g
  
  gsens <- case HS.size (HS.intersection args symSetG) of
                0 -> return HM.empty
                _ -> getSensitivities args g' (sens*dfdg)
  hsens <- case HS.size (HS.intersection args symSetH) of
                0 -> return HM.empty
                _ -> getSensitivities args h' (sens*dfdh)
  unionWithPlus gsens hsens
  



---------------------- heterogenous inputs/outputs ------------------
data a :* b = a :* b deriving Show
infixr 6 :*

class HList a where
  type NumT a
  type DimT a
--  mkNodes :: (NumT a ~ b) => a -> State (FunGraph b c d) (a,[Key])
  mkNodes :: a -> StateT (FunGraph (NumT a) b c) Identity (a,[Key])
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
  
inputs :: HList b => b -> StateT (FunGraph (NumT b) (DimT b) c) Identity b
inputs exprs = do
  (exprs', keys) <- mkNodes exprs
  FunGraph hm im _ outs <- get
  put (FunGraph hm im (getHDim exprs, keys) outs)
  return exprs'

outputs :: HList c => c -> StateT (FunGraph (NumT c) b (DimT c)) Identity c
outputs exprs = do
  (exprs',keys) <- mkNodes exprs
  FunGraph hm im ins _ <- get
  put (FunGraph hm im ins (getHDim exprs,keys))
  return exprs'

inputs_ :: HList b => b -> StateT (FunGraph (NumT b) (DimT b) c) Identity ()
inputs_ exprs = do
  _ <- inputs exprs
  return ()

outputs_ :: HList c => c -> StateT (FunGraph (NumT c) b (DimT c)) Identity ()
outputs_ exprs = do
  _ <- outputs exprs
  return ()

---------------- utility function -----------------
makeFun :: StateT (FunGraph a b c) Identity d -> (d, FunGraph a b c)
makeFun f = runState f emptyFunGraph
