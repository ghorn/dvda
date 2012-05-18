{-# OPTIONS_GHC -Wall #-}
{-# Language GADTs #-}
{-# Language FlexibleContexts #-}
{-# Language TypeOperators #-}
{-# Language TypeFamilies #-}
{-# Language ForeignFunctionInterface #-}

module Dvda.Codegen.CFunction ( run
                              , mkFun
                              , callCFunction
                              , buildCFunction
                              ) where

--import Data.Array.Repa ( DIM0, DIM1, DIM2 )
import Data.Array.Repa ( DIM0 )

import qualified Data.Hashable as H
import qualified Data.HashMap.Lazy as HM
import qualified Data.Vector.Unboxed as V

import System.Directory
import System.Process(runCommand, waitForProcess)
import System.Exit(ExitCode(ExitSuccess))
import Control.Monad(when)

import System.IO.Unsafe(unsafePerformIO)
import Foreign.Marshal.Array(mallocArray, newArray, peekArray)
import Foreign.C.Types(CDouble, CInt(..))
import Foreign.Ptr(Ptr, FunPtr)
import Control.Monad(zipWithM)

import qualified Dvda.Config as Config
import Dvda.SymMonad hiding ( inputs, outputs )
import Dvda.Expr
import Dvda.Graph
import Dvda.Codegen.CSyntax ( writeCSource, writeCInclude )


type CallFunction = Ptr (Ptr CDouble) -> Ptr (Ptr CDouble) -> IO CInt
foreign import ccall "dynamic" mkFun :: FunPtr a -> CallFunction

callCFunction :: (Real a, Fractional a) => [Int] -> FunPtr a -> [[a]] -> [[a]]
callCFunction numOutputs c_fun inputs = unsafePerformIO $ do
  -- set up inputs
  inputArray <- mapM newArray (map (map realToFrac) inputs) >>= newArray
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


-- | shorten path name for display purposes
shortName :: String -> String
shortName full
  | length name <= maxN = name ++ extension
  | otherwise           = take firstN name ++ "..." ++ drop (length name - lastN) name ++ extension
  where
    firstN = 20
    lastN  = 10
    maxN = firstN + lastN

    (name, extension) = break (== '.') $ reverse $ takeWhile (/= '/') (reverse full)


-- | take in name of source and future object, compile object
callGcc :: FilePath -> FilePath -> IO ()
callGcc srcname objname = do
  -- compile new object
  let compileString = Config.gccString srcname objname
      displayString = Config.gccString (shortName srcname) (shortName objname)

  -- print compilation string
  when Config.spewGccCall $ putStrLn displayString
  
  -- run compilation string
  p <- runCommand compileString
  
  -- check for errors
  exitCode <- waitForProcess p
  when (exitCode /= ExitSuccess) $ error $ "failed compiling " ++ srcname


-- | make source functions
--buildCFunction :: (Eq a, Show a) => [Expr a] -> [Expr a] -> IO (String, FilePath)
buildCFunction :: (Show a, V.Unbox a, H.Hashable a) => FunGraph a b c -> IO (String, FilePath)
buildCFunction fg@(FunGraph hm _ _ _) = do
  -- C source and hash
  let hash = show $ abs $ H.hash $ HM.toList hm
      cSource = writeCSource fg hash 
      cInclude = writeCInclude hash

  -- function directory
  dir <- Config.functionDir hash
  
  -- make function directory if it doesn't exist
  createDirectoryIfMissing False dir
  
  -- filenames
  let cSourceFile  = dir ++ "/" ++ Config.nameCSource  hash
      cIncludeFile = dir ++ "/" ++ Config.nameCInclude hash
      cObjectFile  = dir ++ "/" ++ Config.nameCObject  hash
      
  -- make function directory if it doesn't exist
  createDirectoryIfMissing False dir
  
  -- if the source already exists, make sure it matches the old source
  srcExists <- doesFileExist cSourceFile
  when srcExists $ do
    oldSrc <- readFile cSourceFile
    when (cSource /= oldSrc) $ putStrLn $
      "====================================================\n" ++ 
      "WARNING: Hash not unique or source code has been edited\n"++ 
      "If you have not edited the auto-generated C code, please let me\n" ++
      "know that Hash clashes happen IRL at gregmainland@gmail.com\n" ++
      "====================================================\n\n"
  
  -- write c source
  writeFile cSourceFile cSource

  -- write c header
  writeFile cIncludeFile cInclude

  -- compile code
  callGcc cSourceFile cObjectFile
  
  -- return info
  return (hash, cObjectFile)


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

----gr :: FunGraph Double (DIM0 :* DIM0) (DIM0)
--gr :: FunGraph Double (DIM0) (DIM0)
--gr = snd $ makeFun $ do
--  let x = sym "x"
----      y = sym "y"
--  inputs_ (x)-- :* y)
--  
--  z1 <- node $ (x) -- *y)
--  z2 <- node $ diff x x
----  z2 <- node $ (dot z y)**2
----  z2 <- node $ (z*y)**2
----  z3 <- node $ diff ((x*x/2)**x) x
--  
--  outputs_ (z2)

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
