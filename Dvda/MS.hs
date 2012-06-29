{-# OPTIONS_GHC -Wall #-}
{-# Language FlexibleContexts #-}
{-# Language TypeOperators #-}
{-# Language TypeFamilies #-}

module Dvda.MS ( dynamicsErrorsEuler
               , dynamicsErrorsSimpson
               , msCost
               , msConstraints
               , writeMS
               ) where

import qualified Data.IntMap as IM
import Data.Array.Repa ( Z(..) )

import Dvda
import Dvda.Codegen ( writeSourceFile )
import Dvda.Graph ( fromDynamic )
import Dvda.OctaveSyntax ( GenOctave, showOctaveSource )
import Dvda.SparseLA
import Dvda.SymMonad ( rad )
import qualified Dvda.Config as Config

eulerError :: (Fractional a, Num (SparseVec a))
              => ([a] -> [a] -> [a])
              -> a
              -> (SparseVec a, SparseVec a) -> (SparseVec a, SparseVec a)
              -> SparseVec a
eulerError listOde dt (xk,xkp1) (uk,_) = xkp1 - (xk + svScale dt f0)
  where
    ode (SparseVec _ xs) (SparseVec _ us) = svFromList $ listOde (IM.elems xs) (IM.elems us)
    f0 = ode xk uk
              
simpsonsRuleError :: (Fractional a, Num (SparseVec a))
                     => ([a] -> [a] -> [a])
                     -> a
                     -> (SparseVec a, SparseVec a) -> (SparseVec a, SparseVec a)
                     -> SparseVec a
simpsonsRuleError listOde dt (xk,xkp1) (uk,ukp1) = xkp1 - xk - (svScale (dt/6.0) (f0 + fourFm + f1))
  where
    ode (SparseVec _ xs) (SparseVec _ us) = svFromList $ listOde (IM.elems xs) (IM.elems us)

    f0 = ode xk uk
    f1 = ode xkp1 ukp1

    um = svScale 0.5 (uk + ukp1)
    xm = xm' - xm''
      where
        xm' = svScale 0.5 (xk + xkp1)
        xm'' = svScale (0.125*dt) (f1 - f0)

    fm = ode xm um
    fourFm = svScale 4 fm

dynamicsErrorsEuler :: Fractional a => [SparseVec a] -> [SparseVec a] -> ([a] -> [a] -> [a]) -> a -> [SparseVec a]
dynamicsErrorsEuler stateVecs' actionVecs' ode dt = zipWith (eulerError ode dt) xPairs uPairs
  where
    xPairs = zip (init  stateVecs') (tail  stateVecs')
    uPairs = zip (init actionVecs') (tail actionVecs')

dynamicsErrorsSimpson :: Fractional a => [SparseVec a] -> [SparseVec a] -> ([a] -> [a] -> [a]) -> a -> [SparseVec a]
dynamicsErrorsSimpson stateVecs' actionVecs' ode dt = zipWith (simpsonsRuleError ode dt) xPairs uPairs
  where
    xPairs = zip (init  stateVecs') (tail  stateVecs')
    uPairs = zip (init actionVecs') (tail actionVecs')

msConstraints :: [Expr Z Double] -> [Expr Z Double] -> [Expr Z Double] -> [Expr Z Double]
                 -> FunGraph Double ([Expr Z Double] :* [Expr Z Double])
                 ([Expr Z Double] :* [Expr Z Double] :* [[Expr Z Double]] :* [[Expr Z Double]])
msConstraints ceqs_ cineqs_ dvs params = runFunGraph $ do
  ceqs <- mapM node ceqs_
  cineqs <- mapM node cineqs_

  ceqsJacobs_ <- mapM (flip rad dvs) ceqs
  cineqsJacobs_ <- mapM (flip rad dvs) cineqs

  let ceqsJacobs   = map (map (fromDynamic Z)) ceqsJacobs_
      cineqsJacobs = map (map (fromDynamic Z)) cineqsJacobs_

  inputs_ (dvs :* params)
  outputs_ (cineqs :* ceqs :* cineqsJacobs :* ceqsJacobs)


msCost :: Expr Z Double -> [Expr Z Double] -> [Expr Z Double]
          -> FunGraph Double ([Expr Z Double] :* [Expr Z Double])
          (Expr Z Double :* [Expr Z Double])
msCost cost_ dvs params = runFunGraph $ do
  cost <- node cost_

  costGrad_ <- (flip rad dvs) cost

  let costGrad     = map (fromDynamic Z) costGrad_

  inputs_ (dvs :* params)
  outputs_ (cost :* costGrad)

writeMS :: GenOctave c => FilePath -> String
           -> FunGraph Double ([Expr Z Double] :* [Expr Z Double]) c-> IO FilePath
writeMS funDir name fg = do
  let source = showOctaveSource fg name
      sourceName = Config.nameOctaveSource name
  writeSourceFile source funDir sourceName
