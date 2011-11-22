-- Function.hs

{-# OPTIONS_GHC -Wall #-}

module Numeric.Dvda.Function( Function(..)
                            , inputNames
                            , callNative
                            , callC
                            ) where

import Foreign.Ptr(FunPtr)

import Numeric.Dvda.Codegen.CallWrapper(callCFunction)
import Numeric.Dvda.Expr.Apply(substitutes, evaluate)
import Numeric.Dvda.Expr.Expr
import Numeric.Dvda.Expr.SourceType

data Function a = Function { funInputs :: [Expr a]
                           , funOutputs :: [Expr a]
                           , funCFunPtr :: FunPtr a
                           , funHash :: String
                           }



inputNames :: (Eq a, Show a) => Function a -> [String]
inputNames (Function {funInputs = inputs}) = map f inputs
  where
    f (Source (Sym name)) = name
    -- remove after issue 18 is fixed:
    f _ = error "non-source Function input detected in " ++ show inputs

-- SLOW native call of funciton
callNative :: Floating a => Function a -> [Expr a] -> [a]
callNative fun args = map evaluate $ map (\x -> substitutes x subRules)  (funOutputs fun)
  where
    subRules = zip (funInputs fun) args

-- fast c call of function
callC :: RealFrac a => Function a -> [a] -> [a]
callC fun args = callCFunction (length (funInputs fun)) (funCFunPtr fun) args
