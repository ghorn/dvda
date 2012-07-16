{-# OPTIONS_GHC -Wall #-}
{-# Language TypeOperators #-}
{-# Language TypeFamilies #-}
{-# Language FlexibleInstances #-}
{-# Language FlexibleContexts #-}
{-# Language GADTs #-}
{-# Language DoAndIfThenElse #-}

module Dvda.SymMonad ( (:*)(..)
                     , MkFunGraph(..)
                     , node
                     , inputs
                     , inputs_
                     , outputs
                     , outputs_
                     , makeFunGraph
                     , runFunGraph
                     , rad
                     , getSensitivities
                     , recover
                     , fullShow
                     , fullShowNodes
                     , runDeriv
                     ) where

import Control.Monad ( foldM, liftM )
import Control.Monad.State ( State, get, put, runState )
import Data.Array.Repa ( DIM0, DIM1, DIM2, Z(..) )
import Data.Hashable ( Hashable )
import Data.Maybe ( fromJust )
import qualified Data.HashSet as HS
import qualified Data.IntMap as IM
import Numeric.LinearAlgebra ( Element, Vector, Matrix )
import qualified Numeric.LinearAlgebra as LA
-- import Debug.Trace

import Dvda.Dual ( Dual(..), dualPerturbation )
import Dvda.BinUn ( applyUnary, applyBinary )
import Dvda.Graph ( FunGraph(..), DynamicExpr(..), DvdaDim(..), insert, emptyFunGraph, fgLookup, fgExprFromKey )
import Dvda.Expr ( Expr(..), Const(..), Sym(..), dim )
import qualified Dvda.HashMap as HM

---- | take all sub expressions of an Expr and turn them into nodes
----   return an Expr that is just a ref
node :: (Hashable a, Eq a, Floating a, Num (Vector a), LA.Container Vector a, DvdaDim sh) => 
         Expr sh a -> State (FunGraph a b c) (Expr sh a)
node (EDimensionless _) = error "don't put EDimensionless in graph, ya goon"
node (EJacob _ _) = error "can't do node EJacob yet"
node e@(ERef _ _ _) = return e
node e@(EConst _) = return e
node e@(ESym _ (SymDependent _ _ dep)) = do
  _ <- node (ESym Z dep)
  insert e
node e@(ESym _ _) = insert e
node (EUnary op x') = do
  x <- node x'
  insert $ EUnary op x
node (EBinary op x' y') = do
  x <- node x'
  y <- node y'
  insert $ EBinary op x y
node (EScale x' y') = do
  x <- node x'
  y <- node y'
  insert $ EScale x y
node (EDeriv x_ arg_) = do
  x <- node x_
  arg <- node arg_
  outs <- rad x [arg]
  node (head outs)
node (EGrad x_ arg_) = do
  x <- node x_
  arg <- node arg_
  outs <- rad x [arg]
  node (head outs)


-- gradient of expression w.r.t. list of args
rad :: (Eq a, Floating a, Num (Vector a), Hashable a, LA.Container Vector a, DvdaDim sh0, DvdaDim sh) =>
       Expr sh0 a -> [Expr sh a] -> State (FunGraph a b c) [Expr sh a]
rad expr' args' = do
  expr <- node expr'
  args'' <- mapM node args'
  fg <- get

  let args = map (\(ERef sh _ k) -> fromJust $ fgExprFromKey sh k fg) args''
      argSet = HS.fromList (map makeDynamic args)

  sensitivities <- getSensitivities argSet expr (EConst (CSingleton (dim expr) 1))
  -- order inputs requested by user
  
  let getSens arg = case HM.lookup (makeDynamic arg) sensitivities of
        Just sens -> node $ fromDynamic (dim arg) sens
--        Nothing -> trace "WARNING: taking deriviative df/dx where f is not a function of x" $
--                   return $ EConst (CSingleton (dim arg) 0)
        Nothing -> return $ EConst (CSingleton (dim arg) 0)
  mapM getSens args


-- | combine two (DynamicExpr a, DynamicExpr a) hashmaps
-- if there is a conflict, add the two sensitivities together
unionWithPlus :: (Hashable a, Eq a, Num (Vector a), LA.Container Vector a, Floating a) =>
                 HM.HashMap (DynamicExpr a) (DynamicExpr a) -> HM.HashMap (DynamicExpr a) (DynamicExpr a)
                 -> State (FunGraph a b c) (HM.HashMap (DynamicExpr a) (DynamicExpr a))
unionWithPlus xs ys = foldM addCommon union0 commonDExprs
  where
    -- the gexprs that occur in both maps
    commonDExprs = HM.keys $ HM.intersection xs ys
    -- the initial union that needs conflicts fixed
    union0 = xs `HM.union` ys
    addCommon hm commonDExpr = do
      let xsens = fromJust $ HM.lookup commonDExpr xs
          ysens = fromJust $ HM.lookup commonDExpr ys
      xysens <- case (xsens,ysens) of
        (DynamicExpr0 x, DynamicExpr0 y) -> do
          ret <- node (x + y)
          return (makeDynamic ret)
        (DynamicExpr1 x, DynamicExpr1 y) -> do
          ret <- node (x + y)
          return (makeDynamic ret)
        (DynamicExpr2 x, DynamicExpr2 y) -> do
          ret <- node (x + y)
          return (makeDynamic ret)
        (_, _) -> error "unionWithPlus got different dimensions"
      return (HM.insert commonDExpr xysens hm)


lookupSymSet :: (Eq a, Hashable a, Element a, DvdaDim sh) =>
                Expr sh a -> State (FunGraph a b c) (Maybe (HS.HashSet (DynamicExpr a)))
lookupSymSet expr = do
  fg <- get
  case fgLookup expr fg of Just (_,symSet) -> return (Just symSet)
                           Nothing -> return Nothing

getSensitivities :: (Eq a, Floating a, Num (Vector a), Hashable a, LA.Container Vector a, DvdaDim sh) =>
                    HS.HashSet (DynamicExpr a) -> Expr sh a -> Expr sh a
                    -> State (FunGraph a b c) (HM.HashMap (DynamicExpr a) (DynamicExpr a))
getSensitivities _ (EGrad  _ _) _ = error "don't call getSensitivities on EGrad"
getSensitivities _ (EJacob _ _) _ = error "don't call getSensitivities on EJacob"
getSensitivities _ (EDeriv _ _) _ = error "don't call getSensitivities on EDeriv"
getSensitivities _ (EScale _ _) _ = error "cant' do getSensitivities on EScale yet (needs EinSum?)"
getSensitivities _ (EDimensionless _) _ = return HM.empty
getSensitivities _ (EConst _) _         = return HM.empty
getSensitivities args (ERef sh _ k) sens  = do
  fg <- get
  let expr = fromJust $ fgExprFromKey sh k fg
  getSensitivities args expr sens
getSensitivities args primal@(ESym sh (SymDependent name k dep')) sens = do
  let dprimal = makeDynamic primal
      primalMap =
        if HS.member dprimal args
        then HM.fromList [(dprimal, makeDynamic sens)]
        -- don't backprop if there aren't any interesting symbols farther in the tree
        else HM.empty

      dep = ESym sh dep'

  depSymSet <- liftM fromJust $ lookupSymSet dep

  let commonSyms = HS.intersection args depSymSet

  dependentMap <- case HS.size commonSyms of
    0 -> return HM.empty
    _ -> getSensitivities commonSyms dep (sens*primal')
      where
        primal' = ESym sh (SymDependent name (k+1) dep')

  return $ HM.union primalMap dependentMap
getSensitivities args primal@(ESym _ _) sens = do
  let dprimal = makeDynamic primal
  if HS.member dprimal args
  then return $ HM.fromList [(dprimal, makeDynamic sens)]
    -- don't backprop if there aren't any interesting symbols farther in the tree
  else return HM.empty

getSensitivities args (EUnary op g) sens = do
  symSetG <- liftM fromJust $ lookupSymSet g
  case HS.size (HS.intersection args symSetG) of
    -- don't backprop if there aren't any interesting symbols farther in the tree
    0 -> return HM.empty
    _ -> do
      let dfdg = dualPerturbation $ applyUnary op (Dual g 1)
      getSensitivities args g (sens*dfdg)
getSensitivities args (EBinary op g h) sens = do
  symSetG <- lookupSymSet g
  symSetH <- lookupSymSet h
  
  let dfdg = dualPerturbation $ applyBinary op (Dual g 1) (Dual h 0)
      dfdh = dualPerturbation $ applyBinary op (Dual g 0) (Dual h 1)
  
  gsens <- case liftM HS.size (liftM (HS.intersection args) symSetG) of
                Nothing -> return HM.empty
                Just 0 -> return HM.empty
                _ -> getSensitivities args g (sens*dfdg)
  hsens <- case liftM HS.size (liftM (HS.intersection args) symSetH) of
                Nothing -> return HM.empty
                Just 0 -> return HM.empty
                _ -> getSensitivities args h (sens*dfdh)
  unionWithPlus gsens hsens
--getSensitivities args (EScale g h) sens = do
--  symSetG <- lookupSymSet g
--  symSetH <- lookupSymSet h
--  
--  fg <- get
--  let dfdg = h
--      dfdh = g
--  
--  gsens <- case liftM HS.size (liftM (HS.intersection args) symSetG) of
--                Nothing -> return HM.empty
--                Just 0 -> return HM.empty
--                _ -> getSensitivities args g (sens*dfdg)
--  hsens <- case liftM HS.size (liftM (HS.intersection args) symSetH) of
--                Nothing -> return HM.empty
--                0 -> return HM.empty
--                _ -> getSensitivities args h (sens*dfdh)
--  unionWithPlus gsens hsens
--getSensitivities _ (EDeriv _ _) _ = error "don't call getSensitivities on EDeriv"
--getSensitivities _ (EGrad _ _) _  = error "don't call getSensitivities on EGrad"
--getSensitivities _ (EJacob _ _) _ = error "don't call getSensitivities on EJacob"
  


---------------------- heterogenous inputs/outputs ------------------
data a :* b = a :* b deriving Show
infixr 6 :*


---------------------------------- input/output class ---------------------------------------------
class MkFunGraph a where
  type NumT a
  type GenT a
  mkNodes :: a -> State (FunGraph (NumT a) b c) a

instance (Hashable a, Eq a, Floating a, Num (Vector a), LA.Container Vector a) =>
         MkFunGraph (Expr DIM0 a) where
  type NumT (Expr DIM0 a) = a
  type GenT (Expr DIM0 a) = a
  mkNodes = node

instance (Hashable a, Eq a, Floating a, Num (Vector a), LA.Container Vector a) =>
         MkFunGraph (Expr DIM1 a) where
  type NumT (Expr DIM1 a) = a
  type GenT (Expr DIM1 a) = Vector a
  mkNodes = node

instance (Hashable a, Eq a, Floating a, Num (Vector a), LA.Container Vector a) =>
         MkFunGraph (Expr DIM2 a) where
  type NumT (Expr DIM2 a) = a
  type GenT (Expr DIM2 a) = Matrix a
  mkNodes = node

instance (Hashable a, Eq a, Floating a, Num (Vector a), LA.Container Vector a, MkFunGraph (Expr sh a), DvdaDim sh) =>
         MkFunGraph [Expr sh a] where
  type NumT [Expr sh a] = a
  type GenT [Expr sh a] = [GenT (Expr sh a)]
  mkNodes = mapM node

instance (Hashable a, Eq a, Floating a, Num (Vector a), LA.Container Vector a, MkFunGraph (Expr sh a), DvdaDim sh) =>
         MkFunGraph [[Expr sh a]] where
  type NumT [[Expr sh a]] = a
  type GenT [[Expr sh a]] = [[GenT (Expr sh a)]]
  mkNodes = mapM (mapM node)

--instance (Show a, MkFunGraph a) => MkFunGraph [a] where
--  type NumT [a] = NumT a
--  type GenT [a] = [GenT a]
--  type KeyT [a] = [KeyT a]
--  mkNodes xs = do
--    (x',kxs) <- mapM mkNodes xs >>= (return . unzip)
--    return (x', concat kxs)

instance (MkFunGraph a, MkFunGraph b, NumT a ~ NumT b) => MkFunGraph (a :* b) where
  type NumT (a :* b) = NumT a
  type GenT (a :* b) = GenT a :* GenT b
  mkNodes (x :* y) = do
    x' <- mkNodes x
    y' <- mkNodes y
    return (x' :* y')

inputs :: MkFunGraph b => b -> State (FunGraph (NumT b) b c) b
inputs exprs_ = do
  exprs <- mkNodes exprs_
  FunGraph hm im _ outs <- get
  put $ FunGraph hm im exprs outs
  return exprs

outputs :: MkFunGraph c => c -> State (FunGraph (NumT c) b c) c
outputs exprs_ = do
  exprs <- mkNodes exprs_
  FunGraph hm im ins _ <- get
  put $ FunGraph hm im ins exprs
  return exprs

inputs_ :: MkFunGraph b => b -> State (FunGraph (NumT b) b c) ()
inputs_ exprs = do
  _ <- inputs exprs
  return ()

outputs_ :: MkFunGraph c => c -> State (FunGraph (NumT c) b c) ()
outputs_ exprs = do
  _ <- outputs exprs
  return ()

------------------ utility function -----------------
runFunGraph :: State (FunGraph a b c) d -> FunGraph a b c
runFunGraph f = snd $ runState f emptyFunGraph

makeFunGraph :: (MkFunGraph b, MkFunGraph c, NumT b ~ NumT c) =>
                b -> c -> FunGraph (NumT b) b c
makeFunGraph ins outs = runFunGraph $ do
  inputs_ ins
  outputs_ outs

-- | Show an Expr, looking up all ERefs
fullShow :: (Show a, Element a, DvdaDim sh) => FunGraph a b c -> Expr sh a -> String
fullShow fg = show . (recover fg)

fullShowNodes :: (Show a, Element a) => FunGraph a b c -> String
fullShowNodes fg@(FunGraph _ im _ _) =
  init $ unlines $ map (\(a,b) -> show a ++ ": " ++ (fullShow fg) (fromDynamic Z b)) (IM.toList im)

-- | Take a FunGraph and an expression and traverse the expression.
--   .
--   Each time an ERef is found, look it up in the FunGraph and continue traversal
recover :: DvdaDim sh => FunGraph a b c -> Expr sh a -> Expr sh a
recover fg (ERef sh _ k) = recover fg (fromJust $ fgExprFromKey sh k fg)
recover _ e@(EDimensionless _) = e
recover _ e@(ESym _ _) = e
recover _ e@(EConst _) = e
recover fg (EUnary op x) = EUnary op (recover fg x)
recover fg (EBinary op x y) = EBinary op (recover fg x) (recover fg y)
recover fg (EDeriv x y) = EDeriv (recover fg x) (recover fg y)
recover fg (EGrad  x y) = EGrad  (recover fg x) (recover fg y)
recover fg (EJacob  x y) = EJacob  (recover fg x) (recover fg y)
recover fg (EScale  x y) = EScale  (recover fg x) (recover fg y)

-- | "Pure" gradient which which runs rad and then calls recover to substitute values for ERefs
runDeriv :: (Eq a, Floating a, Num (Vector a), Hashable a, LA.Container Vector a, DvdaDim sh)
            => Expr sh a -> [Expr sh a] -> [Expr sh a]
runDeriv expr args = map (recover fg) deda
  where
    (deda, fg) = runState (rad expr args) emptyFunGraph
