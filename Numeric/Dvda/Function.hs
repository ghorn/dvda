-- Function.hs

{-# OPTIONS_GHC -Wall #-}

module Numeric.Dvda.Function( Function(..)
                            , inputNames
                            , callNative
                            , callC
                            , toFunction
                            ) where

import Data.Maybe
import System.Posix.DynamicLinker
import Foreign.Ptr(FunPtr)

import qualified Numeric.Dvda.Config as Config
import Numeric.Dvda.Codegen.CallWrapper(callCFunction)
import Numeric.Dvda.Codegen.Codegen(buildCFunction)
import Numeric.Dvda.Substitute
import Numeric.Dvda.Expr

data Function a = Function { funInputs :: [Expr a]
                           , funOutputs :: [Expr a]
                           , funCFunPtr :: FunPtr a
                           , funHash :: String
                           }


inputNames :: (Eq a, Show a) => Function a -> [String]
inputNames (Function {funInputs = inputs}) = map f inputs
  where
    f x
      | isJust (symName x) = fromJust $ symName x
      | otherwise = error "non-source Function input detected in " ++ show inputs

-- | SLOW native call of function
-- | provided mainly for developing a function or debugging
callNative :: Floating a => Function a -> [Expr a] -> [Expr a]
callNative fun args 
  | length (funInputs fun) == length args = map (eval . (subs subRules)) (funOutputs fun)
  | otherwise = error "callNative fail because num arguments /= num function inputs"
  where
    subRules = zip (funInputs fun) args

-- | fast c call of function
callC :: RealFrac a => Function a -> [a] -> [a]
callC fun args = callCFunction (length (funInputs fun)) (funCFunPtr fun) args


-- | turn inputs/outputs into a function 
-- | generate, compile, and load c code
toFunction :: RealFrac a => [Expr a] -> [Expr a] -> IO (Function a)
toFunction inputs outputs = do
  -- TODO: make sure all inputs are accounted for - issue 19
  (hash, objPath) <- buildCFunction inputs outputs

  dl <- dlopen objPath [RTLD_NOW]
  funptr <- dlsym dl $ Config.nameCFunction hash

  return $ Function { funInputs  = inputs
                    , funOutputs = outputs
                    , funHash    = hash
                    , funCFunPtr = funptr
                    }
