-- CallHaskell.hs

{-# OPTIONS_GHC -Wall #-}

module Numeric.Dvda.Codegen.CallHaskell( callCFunction
                                       ) where

import System.IO.Unsafe(unsafePerformIO)
import Foreign.Marshal.Array(mallocArray, newArray, peekArray)
import Foreign.C.Types(CDouble)
import Foreign.Ptr(Ptr)

callCFunction :: [Double] -> Int -> (Ptr CDouble -> Ptr CDouble -> IO ()) -> [Double]
callCFunction inputs numOutputs c_fun = unsafePerformIO $ do
  inputArray <- newArray ((map realToFrac inputs) :: [CDouble])
  outputArray <- mallocArray numOutputs
  c_fun inputArray outputArray
  outputs <- peekArray numOutputs outputArray
  return ((map realToFrac outputs) :: [Double])
