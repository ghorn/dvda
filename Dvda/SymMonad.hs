{-# OPTIONS_GHC -Wall #-}
{-# Language GADTs #-}
{-# Language FlexibleContexts #-}
{-# Language TypeOperators #-}
{-# Language TypeFamilies #-}
{-# Language MultiParamTypeClasses #-}
{-# Language FlexibleInstances #-}

module Dvda.SymMonad ( (:*)(..)
                     , HList(..)
                     , Exprs
                     , node
                     , node'
                     , inputs
                     , inputs_
                     , outputs
                     , outputs_
                     , makeFunGraph
                     , runFunGraph
                     , rad
                     , getSensitivities
                     ) where

import Control.Monad ( foldM, zipWithM )
import Control.Monad.State ( MonadState, StateT, get, put, liftM, runState )
import Data.Functor.Identity ( Identity )
import Data.Array.Repa ( Shape, Z, (:.) )
import Data.Hashable ( Hashable )
import Data.Maybe ( fromJust )
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
import qualified Data.IntMap as IM
import Data.IntMap ( Key )
import Numeric.LinearAlgebra ( Element, Vector )
import qualified Numeric.LinearAlgebra as LA
import Debug.Trace ( trace )

import Dvda.Dual ( Dual(..), dualPerturbation )
import Dvda.BinUn ( BinOp(..), applyUnary, applyBinary )
import Dvda.Graph ( FunGraph(..), emptyFunGraph, fgReverseLookup, fgGExprFromKey )
import Dvda.GExpr ( GExpr(..), gdim )
import Dvda.Expr ( Expr(..), Const(..), FromGExpr, dim, exprOfGExpr )
import Dvda.HomoDim ( homoOfShape )

-- | take all sub expressions of an Expr and turn them into nodes
--   return an Expr that is just a ref
node :: (Hashable a, Eq a, Floating a, Num (Vector a), LA.Container Vector a, Shape sh) => 
        Expr sh a -> StateT (FunGraph a b c) Identity (Expr sh a)
node expr = liftM (ERef (dim expr)) (node' expr)
            
node' :: (Hashable a, Eq a, Floating a, Num (Vector a), LA.Container Vector a, Shape sh) => 
         Expr sh a -> StateT (FunGraph a b c) Identity Key
node' (EDimensionless _) = error "don't put EDimensionless in graph, ya goon"
node' (ERef _ k) = return k
node' (ESym sh name) = insert $ GSym (homoOfShape sh) name
node' (EConst (CMat    sh x)) = insert $ GMat    (homoOfShape sh) x
node' (EConst (CVec    sh x)) = insert $ GVec    (homoOfShape sh) x
node' (EConst (CTensor sh x)) = insert $ GTensor (homoOfShape sh) x
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
  let shx = homoOfShape $ dim x
      shy = homoOfShape $ dim y
  insert $ GDot shx shy xk yk
node' (EDeriv x' arg') = do
  x <- node x'
  arg <- node arg'
  outs <- rad x [arg]
  node' (head outs)
node' (EGrad x' arg') = do
  x <- node x'
  arg <- node arg'
  outs <- rad x [arg]
  node' (head outs)


-- | Try to insert the GExpr into the hashmap performing CSE.
--   If the GExpr is not yet in the map, insert it and return new key.
--   Otherwise don't insert, just return existing key.
insert :: (Hashable a, Eq a, Num (Vector a), LA.Container Vector a) => GExpr a -> StateT (FunGraph a b c) Identity Key
insert gexpr = do
  fg <- get
  let symSet (GSym _ _)          = HS.singleton gexpr
      symSet (GSingleton _ _)    = HS.empty
      symSet (GVec _ _)          = HS.empty
      symSet (GMat _ _)          = HS.empty
      symSet (GTensor _ _)       = HS.empty
      symSet (GUnary _ _ k)      = snd $ fromJust $ fgReverseLookup k fg
      symSet (GBinary _ _ xk yk) = symMapX `HS.union` symMapY
        where
          (_,symMapX) = fromJust $ fgReverseLookup xk fg
          (_,symMapY) = fromJust $ fgReverseLookup yk fg
      symSet (GScale _ xk yk) = symMapX `HS.union` symMapY
        where
          (_,symMapX) = fromJust $ fgReverseLookup xk fg
          (_,symMapY) = fromJust $ fgReverseLookup yk fg
      symSet (GDot _ _ xk yk) = symMapX `HS.union` symMapY
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


gexprOfExpr :: (Hashable a, Eq a, Floating a, Num (Vector a), LA.Container Vector a, Shape sh, FromGExpr sh) =>
               Expr sh a -> StateT (FunGraph a b c) Identity (GExpr a)
gexprOfExpr expr = do
  k <- node' expr
  fg <- get
  return (fromJust $ fgGExprFromKey k fg)
  
-- gradient of expression w.r.t. list of args
rad :: (Hashable a, Eq a, Floating a, Num (Vector a), LA.Container Vector a, Shape sh, Shape sh0, FromGExpr sh, FromGExpr sh0) => 
       Expr sh0 a -> [Expr sh a] -> StateT (FunGraph a b c) Identity [Expr sh a]
rad expr_ args_ = do
  expr <- gexprOfExpr expr_
  args <- mapM gexprOfExpr args_
  let argSet = HS.fromList args
  sensitivities <- getSensitivities argSet expr (ESingleton (dim expr_) 1)
  -- order inputs requested by user
  let getSens x argDim = case HM.lookup x sensitivities of
        Just sens -> return sens
        Nothing -> trace "WARNING: taking deriviative df/dx where f is not a function of x (inserting 0 in graph)" $
                   node' (ESingleton argDim 0)
      argDims = map dim args_
  orderedSensitivities <- zipWithM getSens args argDims
  return $ zipWith ERef argDims orderedSensitivities


-- combine two (GExpr, Key) hashmaps
-- if there is a conflict, add the two GExprs together
unionWithPlus :: (Hashable a, Eq a, Num (Vector a), LA.Container Vector a) => 
                 HM.HashMap (GExpr a) Key -> HM.HashMap (GExpr a) Key ->
                 StateT (FunGraph a b c) Identity (HM.HashMap (GExpr a) Key)
unionWithPlus xs ys = foldM addCommon union0 commonGExprs
  where
    -- the gexprs that occur in both maps
    commonGExprs = HM.keys $ HM.intersection xs ys
    -- the initial union that needs conflicts fixed
    union0 = xs `HM.union` ys
    addCommon hm commonGExpr = do
      let xsensk = fromJust $ HM.lookup commonGExpr xs
          ysensk = fromJust $ HM.lookup commonGExpr ys
      k <- insert $ GBinary (gdim commonGExpr) Add xsensk ysensk
      return (HM.insert commonGExpr k hm)
              

lookupSymSet :: (Eq a, Hashable a, Element a) => Key -> StateT (FunGraph a b c) Identity (HS.HashSet (GExpr a))
lookupSymSet k = do
  fg <- get
  let (_,symSet) = fromJust $ fgReverseLookup k fg
  return symSet


getSensitivities :: (Hashable a, Eq a, Floating a, Num (Vector a), LA.Container Vector a, Shape sh, FromGExpr sh) =>
                     HS.HashSet (GExpr a) -> GExpr a -> Expr sh a ->
                     StateT (FunGraph a b c) Identity (HM.HashMap (GExpr a) Key)
getSensitivities _ (GSingleton _ _) _ = return HM.empty
getSensitivities _ (GVec _ _) _ = return HM.empty
getSensitivities _ (GMat _ _) _ = return HM.empty
getSensitivities _ (GTensor _ _) _ = return HM.empty
getSensitivities args primal@(GSym _ _) sens = if HS.member primal args then do
  k <- node' sens
  return $ HM.fromList [(primal, k)]
  -- don't backprop if there aren't any interesting symbols farther in the tree
  else return HM.empty
getSensitivities args (GUnary _ op gk) sens = do
  symSetG <- lookupSymSet gk
  case HS.size (HS.intersection args symSetG) of
    -- don't backprop if there aren't any interesting symbols farther in the tree
    0 -> return HM.empty
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
getSensitivities args (GDot _ _ gk hk) sens = do
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
  type DimT (a :* b) = DimT a :* DimT b
  mkNodes (x :* y) = do
    (exs,kxs) <- mkNodes x
    (eys,kys) <- mkNodes y
    return (exs :* eys, kxs++kys)
  getHDim (x :* y) = getHDim x :* getHDim y

instance (Shape sh, Hashable a, Eq a, Floating a, Num (Vector a), LA.Container Vector a) => 
         HList (Expr sh a) where
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

--------------------------------------------------------------
class ExprList sh a where
  type Exprs sh a
  
instance (ExprList sh0 a, ExprList sh1 a) => ExprList (sh0 :* sh1) a where
  type Exprs (sh0 :* sh1) a = (Exprs sh0 a) :* (Exprs sh1 a)
      
instance ExprList Z a where
  type Exprs Z a = Expr Z a

instance Shape sh => ExprList (sh :. Int) a where
  type Exprs (sh :. Int) a = Expr (sh :. Int) a


---------------- utility function -----------------
runFunGraph :: StateT (FunGraph a b c) Identity d -> FunGraph a b c
runFunGraph f = snd $ runState f emptyFunGraph

--makeFunGraph :: (HList c, HList b, NumT b ~ NumT c, NumT b ~ a, Eq a, Floating a, Hashable a) =>
makeFunGraph :: (HList c, HList b, NumT b ~ NumT c, NumT b ~ a) =>
                b -> c -> FunGraph a (DimT b) (DimT c)
makeFunGraph ins outs = runFunGraph $ do
  inputs_ ins
  outputs_ outs
