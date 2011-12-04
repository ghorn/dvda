{- |
   Module      : Numeric.Dvda.Function
   Description : Interface for numerically evaluating `Numeric.Dvda.Expr.Expr`

   Turn  `Numeric.Dvda.Expr.Expr`s into functions callable through automatically generated C code or through a slow native function.
 -}

{-# OPTIONS_GHC -Wall #-}

module Numeric.Dvda.Function( -- * Creation
                              toFunction
                            , Function
                              -- * Calling
                            , callC
--                            , callCL
                            , callNative
                            ) where

import Data.List(nub)
import System.Posix.DynamicLinker
import Foreign.Ptr(FunPtr)
import Control.Monad(when)

import qualified Numeric.Dvda.Config as Config
import Numeric.Dvda.Codegen.CallWrapper(callCFunction)
import Numeric.Dvda.Codegen.Codegen(buildCFunction)
import Numeric.Dvda.Symbolic
import Numeric.Dvda.Internal.Expr
import Numeric.Dvda.Internal.Tensor
import Numeric.Dvda.Internal.ExprUtils

data Function a = Function { funInputs :: [Expr a]
                           , funOutputs :: [Expr a]
                           , funCFunPtr :: FunPtr a
                           , funHash :: String
                           }

-- | Call a function without using C code by recursively evaluating children.
--   This is probably much slower than 'Numeric.Dvda.Function.callC' and
--   is intended mainly for developing a function or debugging.
callNative :: Floating a => Function a -> [Expr a] -> [Expr a]
callNative fun args 
  | length (funInputs fun) == length args = map (evalE . subs subRules) (funOutputs fun)
  | otherwise = error "callNative fail because num arguments /= num function inputs"
  where
    subRules = zip (funInputs fun) args


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

-- | Call a function using generated C code with `Numeric.Dvda.Expr.Expr` inputs and outputs
callC :: (Floating a, Real a) => Function a -> [Expr a] -> [Expr a]
callC fun inputs
  | and (zipWith (==) trueInputLengths userInputLengths) = map tensorToExpr $ zipWith TNum outputDims listOutputs
  | otherwise = error $ "callC detected improper number of inputs\n"++
                "expected input lengths: " ++ show trueInputLengths ++ "\n" ++
                "user input lengths:     " ++ show userInputLengths
  where
    outputLengths = map product outputDims
    outputDims = map dim (funOutputs fun)
    trueInputLengths = map (product . dim) (funInputs fun)    
    userInputLengths = map (product . dim) inputs
    
    listOutputs = callCFunction outputLengths (funCFunPtr fun) (map (snd . eval) inputs)


-- | Turn lists of inputs and outputs into a function
toFunction :: RealFrac a => [Expr a] -> [Expr a] -> IO (Function a)
toFunction inputs outputs = do
  let symsInOutputs = nub $ concatMap getSyms outputs
      missingInputs = filter (`notElem` inputs) symsInOutputs

  when (length missingInputs > 0) $
    error $ "in outputs: "++show outputs++"\ntoFunction detected missing inputs: " ++ show missingInputs
  
  (hash, objPath) <- buildCFunction inputs outputs
  
  dl <- dlopen objPath [RTLD_NOW]
  funptr <- dlsym dl $ Config.nameCFunction hash

  return Function { funInputs  = inputs
                  , funOutputs = outputs
                  , funHash    = hash
                  , funCFunPtr = funptr
                  }
