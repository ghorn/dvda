{- |
   Module      : Dvda.Function
   Description : Interface for numerically evaluating `Dvda.Graph.FunGraph`

   Turn  `Numeric.Dvda.Expr.Expr`s into functions callable through automatically generated C code or through a slow native function.
 -}

{-# OPTIONS_GHC -Wall #-}

{-# Language GADTs #-}
{-# Language FlexibleContexts #-}
{-# Language TypeOperators #-}
{-# Language TypeFamilies #-}

module Dvda.CFunction ( -- * Creation
                        toFunction
                      , CFunction
                        -- * Calling
--                      , callC
--                      , callCL
                      ) where

import Data.Array.Repa ( DIM0 )
--import Data.List ( nub )
import System.Posix.DynamicLinker
import Foreign.Ptr ( FunPtr )
--import Control.Monad ( when )
import Data.Hashable ( Hashable )
import Data.Vector.Unboxed ( Unbox )

import qualified Dvda.Config as Config
import Dvda.Codegen.CCallWrapper ( callCFunction )
import Dvda.Codegen.CBuilder ( buildCFunction )
import Dvda.Graph
import Dvda.SymMonad
import Dvda.Expr

data CFunction a b c = CFunction { cfunGraph :: FunGraph a b c
                                 , cfunCFunPtr :: FunPtr a
                                 , cfunHash :: String
                                 }

---- | Call a function using generated C code with list inputs/outputs
--callCL :: RealFrac a => Function a -> [[a]] -> [[a]]
--callCL fun inputs
--  | and (zipWith (==) trueInputLengths userInputLengths) = callCFunction trueOutputLengths (funCFunPtr fun) inputs
--  | otherwise = error $ "callCL detected improper number of inputs\n"++
--                "expected input lengths: " ++ show trueInputLengths ++ "\n" ++
--                "user input lengths:     " ++ show userInputLengths
--  where
--    trueOutputLengths = map (product . dim) (funOutputs fun)
--    trueInputLengths = map (product . dim) (funInputs fun)    
--    userInputLengths = map length inputs

---- | Call a function using generated C code with `Numeric.Dvda.Expr.Expr` inputs and outputs
----callC :: (Floating a, Real a) => CFunction a b c -> b -> c
--callC fun inputs = map Expr $ zipWith TNum outputDims listOutputs
----  | and (zipWith (==) trueInputLengths userInputLengths) = map Expr $ zipWith TNum outputDims listOutputs
----  | otherwise = error $ "callC detected improper number of inputs\n"++
----                "expected input lengths: " ++ show trueInputLengths ++ "\n" ++
----                "user input lengths:     " ++ show userInputLengths
----  where
----    outputLengths = map dsize outputDims
----    outputDims = map dim (funOutputs fun)
----    trueInputLengths = map (dsize . dim) (funInputs fun)    
----    userInputLengths = map (dsize . dim) inputs
--    
--    listOutputs = callCFunction outputLengths (cfunCFunPtr fun) (map (snd . eval) inputs)


-- | Turn lists of inputs and outputs into a function
toFunction :: (Hashable a, Unbox a, RealFrac a, Show a) => FunGraph a b c -> IO (CFunction a b c)
toFunction fg@(FunGraph hm im _ _) = do
--  let symsInOutputs = nub $ concatMap getSyms outputs
--      missingInputs = filter (`notElem` inputs) symsInOutputs
--
--  when (any (not . isSymbolic) inputs) $
--    error $ "toFunction detected non-symbolic inputs: " ++ show (filter (not . isSymbolic) inputs) ++ "\nHave you accidentally switched inputs and outputs?"
--  when (length missingInputs > 0) $
--    error $ "in outputs: "++show outputs++"\ntoFunction detected missing inputs: " ++ show missingInputs
  
  (hash, objPath) <- buildCFunction fg
  
  dl <- dlopen objPath [RTLD_NOW]
  funptr <- dlsym dl $ Config.nameCFunction hash

  return CFunction { cfunGraph = fg
                   , cfunHash    = hash
                   , cfunCFunPtr = funptr
                   }



--gr :: FunGraph Double (DIM0 :* DIM1 :* DIM2) (DIM2 :* DIM1 :* DIM0)
gr :: FunGraph Double (DIM0 :* DIM0 :* DIM0) (DIM0 :* DIM0 :* DIM0)
gr = snd $ makeFun $ do
  let x = sym "x"
--      y = vsym 5 "y"
      y = sym "y"
--      z = msym (3,5) "Z"
      z = sym "Z"
  inputs_ (x :* z :* y)
  
  z1 <- node $ (scale x z)**3
--  z2 <- node $ (dot z y)**2
  z2 <- node $ (z*y)**2
--  z3 <- node $ diff ((x*x/2)**x) x
  z3 <- node $ ((x*x/2)**x)*x
  
  outputs_ (z1 :* z2 :* z3)


run :: IO ()
run = do
--  putStrLn $ funGraphSummary gr
--  putStrLn $ funGraphSummary' gr
--  putStrLn $ showCollisions gr

  (hash, cObjectFile) <- buildCFunction gr
  print hash
  print cObjectFile

--  print cObjectFile
--  previewGraph gr
