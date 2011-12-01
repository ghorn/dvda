-- CallWrapper.hs

{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE ForeignFunctionInterface #-}

module Numeric.Dvda.Codegen.CallWrapper( callCFunction
                                       ) where

import System.IO.Unsafe(unsafePerformIO)
import Foreign.Marshal.Array(mallocArray, newArray, peekArray)
import Foreign.C.Types(CDouble, CInt)
import Foreign.Ptr(Ptr, FunPtr)

type CallFunction = Ptr CDouble -> Ptr CDouble -> IO CInt
foreign import ccall "dynamic" mkFun :: FunPtr a -> CallFunction

callCFunction :: (Real a, Fractional a) => Int -> FunPtr a -> [a] -> [a]
callCFunction numOutputs c_fun inputs = unsafePerformIO $ do
  inputArray <- newArray (map realToFrac inputs :: [CDouble])
  outputArray <- mallocArray numOutputs :: IO (Ptr CDouble)
  
  let c_call = mkFun c_fun
  _ <- c_call inputArray outputArray
  
  outputs <- peekArray numOutputs outputArray
  return (map realToFrac outputs)
