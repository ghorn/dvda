{- |
   Module      : Numeric.Dvda.Function
   Description : Interface for numerically evaluating `Numeric.Dvda.Expr.Expr`

   Turn  `Numeric.Dvda.Expr.Expr`s into functions callable through automatically generated C code or through a slow native function.
 -}

{-# OPTIONS_GHC -Wall #-}

module Numeric.Dvda.Function( toFunction
                            , Function(..)
                            , callC
                            , callNative
                            , inputNames
                            ) where

import Data.Maybe
import System.Posix.DynamicLinker
import Foreign.Ptr(FunPtr)

import qualified Numeric.Dvda.Config as Config
import Numeric.Dvda.Codegen.CallWrapper(callCFunction)
import Numeric.Dvda.Codegen.Codegen(buildCFunction)
import Numeric.Dvda.Symbolic
import Numeric.Dvda.Internal.Expr
import Numeric.Dvda.Internal.ExprUtils

data Function a = Function { funInputs :: [Expr a]
                           , funOutputs :: [Expr a]
                           , funCFunPtr :: FunPtr a
                           , funHash :: String
                           }

-- | Get a list of names of a function's inputs
inputNames :: (Eq a, Show a) => Function a -> [String]
inputNames (Function {funInputs = inputs}) = map f inputs
  where
    f x
      | isJust (symName x) = fromJust $ symName x
      | otherwise = error "non-source Function input detected in " ++ show inputs

-- | Call a function without using C code by recursively evaluating children.
--   This is probably much slower than 'Numeric.Dvda.Function.callC' and
--   is intended mainly for developing a function or debugging.
callNative :: Floating a => Function a -> [Expr a] -> [Expr a]
callNative fun args 
  | length (funInputs fun) == length args = map (eval . subs subRules) (funOutputs fun)
  | otherwise = error "callNative fail because num arguments /= num function inputs"
  where
    subRules = zip (funInputs fun) args

-- | Call a function using generated C code
callC :: RealFrac a => Function a -> [a] -> [a]
callC fun = callCFunction (length (funInputs fun)) (funCFunPtr fun)


-- | Turn lists of inputs and outputs into a function
toFunction :: RealFrac a => [Expr a] -> [Expr a] -> IO (Function a)
toFunction inputs outputs = do
  -- TODO: make sure all inputs are accounted for - issue 19
  (hash, objPath) <- buildCFunction inputs outputs

  dl <- dlopen objPath [RTLD_NOW]
  funptr <- dlsym dl $ Config.nameCFunction hash

  return Function { funInputs  = inputs
                  , funOutputs = outputs
                  , funHash    = hash
                  , funCFunPtr = funptr
                  }
