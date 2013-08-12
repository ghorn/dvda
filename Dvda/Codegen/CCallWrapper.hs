{-# OPTIONS_GHC -Wall #-}
-- {-# Language GADTs #-}
-- {-# Language FlexibleContexts #-}
-- {-# Language TypeOperators #-}
-- {-# Language TypeFamilies #-}
-- {-# Language ForeignFunctionInterface #-}

module Dvda.Codegen.CCallWrapper ( mkFun
                                 , callCFunction
                                 ) where

import System.IO.Unsafe(unsafePerformIO)
import Foreign.Marshal.Array(mallocArray, newArray, peekArray)
import Foreign.C.Types(CDouble, CInt(..))
import Foreign.Ptr(Ptr, FunPtr)
import Control.Monad(zipWithM)

type CallFunction = Ptr (Ptr CDouble) -> Ptr (Ptr CDouble) -> IO CInt
foreign import ccall "dynamic" mkFun :: FunPtr a -> CallFunction

callCFunction :: (Real a, Fractional a) => [Int] -> FunPtr a -> [[a]] -> [[a]]
callCFunction numOutputs c_fun inputs = unsafePerformIO $ do
  -- set up inputs
  inputArray <- mapM (newArray . map realToFrac) inputs >>= newArray
--  listOfPtrs <- mapM newArray (map (map realToFrac) inputs)
--  inputArray <- newArray listOfPtrs
  
  -- malloc output memory
  listOfOutputs <- mapM mallocArray numOutputs :: IO [Ptr CDouble]
  outputArray <- newArray listOfOutputs
  
  -- call function
  let c_call = mkFun c_fun
  _ <- c_call inputArray outputArray
  
  -- get outputs
  outputPtrs <- peekArray (length numOutputs) outputArray
  outputs <- zipWithM peekArray numOutputs outputPtrs

  return $ map (map realToFrac) outputs
