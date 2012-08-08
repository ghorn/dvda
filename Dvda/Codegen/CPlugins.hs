{-# OPTIONS_GHC -Wall #-}

module Dvda.Codegen.CPlugins ( loadC
                             ) where

--import System.Process(runCommand, waitForProcess)
--import System.Exit(ExitCode(ExitSuccess))
--import Control.Monad(when)
--import System.IO.Unsafe(unsafePerformIO)
--import Foreign.Marshal.Array(mallocArray, newArray, peekArray)
--import Foreign.C.Types(CDouble, CInt(..))
import Foreign.Ptr(FunPtr)
--import Control.Monad(zipWithM)
--import Data.List(nub)
import System.Posix.DynamicLinker
--import Foreign.Ptr(FunPtr)
--import Control.Monad(when)

--type CallFunction = Ptr (Ptr CDouble) -> Ptr (Ptr CDouble) -> IO CInt
--foreign import ccall "dynamic" mkFun :: FunPtr a -> CallFunction
data CFunction a = CFunction { funInputDims :: [(Int,Int)]
                             , funOutputDims :: [(Int,Int)]
                             , funName :: String
                             , funCFunPtr :: FunPtr a
                             }

-- | Load an object
loadC :: FilePath -- ^ object file
         -> String -- ^ function symbol name
         -> [(Int,Int)] -- ^ input dimensions
         -> [(Int,Int)] -- ^ output dimensions
         -> IO (CFunction a)
loadC objPath symName indims outdims = do
  dl <- dlopen objPath [RTLD_NOW]
  funptr <- dlsym dl symName
  return $ CFunction { funInputDims  = indims
                     , funOutputDims = outdims
                     , funName    = symName
                     , funCFunPtr = funptr
                     }

---- | Call a function using generated C code with `Numeric.Dvda.Expr.Expr` inputs and outputs
--callC :: (Floating a, Real a) => Function a -> [Expr a] -> [Expr a]
--callC fun inputs
--  | and (zipWith (==) trueInputLengths userInputLengths) = map Expr $ zipWith TNum outputDims listOutputs
--  | otherwise = error $ "callC detected improper number of inputs\n"++
--                "expected input lengths: " ++ show trueInputLengths ++ "\n" ++
--                "user input lengths:     " ++ show userInputLengths
--  where
--    outputLengths = map dsize outputDims
--    outputDims = map dim (funOutputs fun)
--    trueInputLengths = map (dsize . dim) (funInputs fun)    
--    userInputLengths = map (dsize . dim) inputs
--    
--    listOutputs = callCFunction outputLengths (funCFunPtr fun) (map (snd . eval) inputs)
--
--
--type CallFunction = Ptr (Ptr CDouble) -> Ptr (Ptr CDouble) -> IO CInt
--foreign import ccall "dynamic" mkFun :: FunPtr a -> CallFunction
--
--callCFunction :: (Real a, Fractional a) => [Int] -> FunPtr a -> [[a]] -> [[a]]
--callCFunction numOutputs c_fun inputs = unsafePerformIO $ do
--  -- set up inputs
--  inputArray <- mapM newArray (map (map realToFrac) inputs) >>= newArray
----  listOfPtrs <- mapM newArray (map (map realToFrac) inputs)
----  inputArray <- newArray listOfPtrs
--  
--  -- malloc output memory
--  listOfOutputs <- mapM mallocArray numOutputs :: IO [Ptr CDouble]
--  outputArray <- newArray listOfOutputs
--  
--  -- call function
--  let c_call = mkFun c_fun
--  _ <- c_call inputArray outputArray
--  
--  -- get outputs
--  outputPtrs <- peekArray (length numOutputs) outputArray
--  outputs <- zipWithM peekArray numOutputs outputPtrs
--
--  return $ map (map realToFrac) outputs
