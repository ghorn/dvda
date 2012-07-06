{-# OPTIONS_GHC -Wall #-}
{-# Language TypeOperators #-}
{-# Language TypeFamilies #-}
{-# Language FlexibleContexts #-}
{-# Language FlexibleInstances #-}
{-# Language GADTs #-}

module Dvda.CallNative ( toNative
                       , nativeDiff
                       , nativeGrad
                       , nativeJacob
                       ) where

import Data.Hashable ( Hashable )
import Data.HashMap.Lazy ( HashMap )
import qualified Data.HashMap.Lazy as HM
import qualified Data.IntMap as IM
import Data.List ( mapAccumL )
import Data.Maybe ( fromJust, catMaybes )
import Numeric.LinearAlgebra ( Element, Container )

import Dvda
import Dvda.BinUn ( BinOp(Mul), applyBinary, applyUnary )
import Dvda.Expr ( Expr(..), Const(..), dim )
import Dvda.Graph ( FunGraph(..), DvdaDim(..), DynamicExpr, fgLookup, fgExprFromKey )
import Dvda.SymMonad ( rad )

class (Hashable (INumT b), Eq (INumT b), Element (INumT b)) => NativeInputs b where
  type INumT b
  toReplacements :: FunGraph (INumT b) b c -> b -> HashMap (DynamicExpr (INumT b)) (DynamicExpr (INumT b))

insToSyms :: DvdaDim sh => FunGraph a b c -> Expr sh a -> Expr sh a -> Maybe (DynamicExpr a, DynamicExpr a)
insToSyms fg e@(ERef _ k) out = fmap (\x -> (makeDynamic x, makeDynamic out)) $ fgExprFromKey (dim e) k fg
insToSyms _ _ _ = Nothing

instance (DvdaDim sh, Hashable a, Element a, Eq a) => NativeInputs (Expr sh a) where
  type INumT (Expr sh a) = a
  toReplacements fg@(FunGraph _ _ ins _) xs = HM.fromList $ catMaybes [insToSyms fg ins xs]

instance (DvdaDim sh, Hashable a, Element a, Eq a) => NativeInputs [Expr sh a] where
  type INumT [Expr sh a] = a
  toReplacements fg@(FunGraph _ _ ins _) xs = HM.fromList $ catMaybes $ zipWith (insToSyms fg) ins xs

instance (DvdaDim sh, Hashable a, Element a, Eq a) => NativeInputs [[Expr sh a]] where
  type INumT [[Expr sh a]] = a
  toReplacements fg@(FunGraph _ _ ins _) xs =
    HM.fromList $ catMaybes $ zipWith (insToSyms fg) (concat ins) (concat xs)

instance (NativeInputs a, NativeInputs b, INumT a ~ INumT b) => NativeInputs (a :* b) where
  type INumT (a :* b) = INumT a
  toReplacements (FunGraph hm im (in0 :* in1) outs) (x0 :* x1) = HM.union r0 r1
    where
      r0 = toReplacements (FunGraph hm im in0 outs) x0
      r1 = toReplacements (FunGraph hm im in1 outs) x1

---------------------------------------------------------------------------
class NativeOutput c where
  type ONumT c
  traverseOutputs :: (NativeInputs b)
                     => HashMap (DynamicExpr (ONumT c)) (DynamicExpr (ONumT c))
                     -> FunGraph (ONumT c) b c
                     -> c
                     -> (FunGraph (ONumT c) b c, c)

instance (DvdaDim sh, Floating a, Num (Vector a), Container Vector a, Hashable a, Eq a)
         => NativeOutput (Expr sh a) where
  type ONumT (Expr sh a) = a
  traverseOutputs = eval

instance (DvdaDim sh, Floating a, Num (Vector a), Container Vector a, Hashable a, Eq a)
         => NativeOutput [Expr sh a] where
  type ONumT [Expr sh a] = a
  traverseOutputs = mapAccumL . eval

instance (DvdaDim sh, Floating a, Num (Vector a), Container Vector a, Hashable a, Eq a)
         => NativeOutput [[Expr sh a]] where
  type ONumT [[Expr sh a]] = a
  traverseOutputs = mapAccumL . mapAccumL . eval

instance (NativeOutput a, NativeOutput b, ONumT a ~ ONumT b) => NativeOutput (a :* b) where
  type ONumT (a :* b) = ONumT a
  traverseOutputs replacementMap (FunGraph hm0 im0 ins outs) (x' :* y') = (FunGraph hm2 im2 ins outs, x :* y)
    where
      err = error "DON'T LOOK AT THESE OUTPUTS YA GOON"
      (FunGraph hm1 im1 _ _, x) = traverseOutputs replacementMap (FunGraph hm0 im0 ins err) x'
      (FunGraph hm2 im2 _ _, y) = traverseOutputs replacementMap (FunGraph hm1 im1 ins err) y'


replace :: (Hashable a, Eq a, Element a, DvdaDim sh) => FunGraph a b c -> Expr sh a -> Expr sh a -> FunGraph a b c
replace fg0@(FunGraph hm0 im0 ins outs) old new = FunGraph hm im ins outs
  where
    (k, _) = fromJust $ fgLookup old fg0
    hm = HM.insert (makeDynamic new) (k, error "after callNative has happened you can't look at symSets") hm0
    im = IM.insert k (makeDynamic new) im0
  

eval :: (Hashable a, Eq a, Floating a, Num (Vector a), Container Vector a, DvdaDim sh)
        => HashMap (DynamicExpr a) (DynamicExpr a) -> FunGraph a b c -> Expr sh a -> (FunGraph a b c, Expr sh a)
eval _ _ (EDimensionless _) = error "WHO PUT AN EDimensionless IN THIS GRAPH"
eval _ _ (EDeriv _ _) = error "WHO PUT AN EDeriv IN THIS GRAPH"
eval _ _ (EGrad _ _) = error "WHO PUT AN EDeriv IN THIS GRAPH"
eval _ _ (EJacob _ _) = error "WHO PUT AN EJacob IN THIS GRAPH"
eval replacementMap fg expr@(ERef _ k) = eval replacementMap fg (fromJust $ fgExprFromKey (dim expr) k fg)
eval _ fg expr@(EConst _) = (fg, expr)
eval replacementMap fg0 expr@(ESym _ _) = case HM.lookup (makeDynamic expr) replacementMap of
 Nothing -> (fg0, expr)
 Just replacementExpr' -> (fg1, replacementExpr)
   where
     replacementExpr = fromDynamic (dim expr) replacementExpr'
     fg1 = replace fg0 expr replacementExpr
eval replacementMap fg0 expr@(EUnary op x') = (fg2, newExpr)
  where
    (fg1, x) = eval replacementMap fg0 x'
    newExpr = applyUnary op x
    fg2 = replace fg1 expr newExpr
eval replacementMap fg0 expr@(EBinary op x' y') = (fg3, newExpr)
  where
    (fg1, x) = eval replacementMap fg0 x'
    (fg2, y) = eval replacementMap fg1 y'
    newExpr = applyBinary op x y
    fg3 = replace fg2 expr newExpr
eval replacementMap fg (EScale (EConst (CSingleton _ x)) y) = eval replacementMap fg z
  where
    z = applyBinary Mul (EConst (CSingleton (dim y) x)) y
eval replacementMap fg0 expr@(EScale x' y') = (fg3, newExpr)
  where
    (fg1, x) = eval replacementMap fg0 x'
    (fg2, y) = eval replacementMap fg1 y'
    newExpr = case x of EConst (CSingleton _ c) -> applyBinary Mul (EConst (CSingleton (dim y) c)) y
                        _ -> EScale x y
    fg3 = replace fg2 expr newExpr

toNative :: (Show a, NativeInputs b, NativeOutput c, a ~ INumT b, a ~ ONumT c) => FunGraph a b c -> b -> c
toNative fg@(FunGraph _ _ _ outs) xs = snd $ traverseOutputs replacementMap fg outs
 where
   replacementMap = toReplacements fg xs


-- | Convenience function for natively computing jacobian, requires you to pass the number of inputs.
--   This is expected to be very slow. Using code generation instead is recommended
nativeJacob :: (Hashable a, Eq a, Show a, Element a, Floating a, Num (Vector a), Container Vector a)
               => Int -> ([Expr Z a] -> [Expr Z a]) -> [Expr Z a] -> [[Expr Z a]]
nativeJacob n f = toNative $ runFunGraph $ do
  let xs = map (\k -> sym ("x_"++show k)) [0..(n-1::Int)]
  inputs_ xs
  ys <- mapM (flip rad xs) (f xs)
  outputs_ ys

-- | Convenience function for natively computing gradient, requires you to pass the number of inputs.
--   This is expected to be very slow. Using code generation instead is recommended
nativeGrad :: (Hashable a, Eq a, Show a, Element a, Floating a, Num (Vector a), Container Vector a)
              => Int -> ([Expr Z a] -> Expr Z a) -> [Expr Z a] -> [Expr Z a]
nativeGrad n f = toNative $ runFunGraph $ do
  let xs = map (\k -> sym ("x_"++show k)) [0..(n-1::Int)]
  inputs_ xs
  ys <- rad (f xs) xs
  outputs_ ys
  
-- | Convenience function for natively computing a derivative.
--   This is expected to be very slow. Using code generation instead is recommended
nativeDiff :: (Hashable a, Eq a, Show a, Element a, Floating a, Num (Vector a), Container Vector a)
              => (Expr Z a -> Expr Z a) -> Expr Z a -> Expr Z a
nativeDiff f = toNative $ runFunGraph $ do
  let x = sym "x"
  inputs_ x
  [y] <- rad (f x) [x]
  outputs_ y
