{-# OPTIONS_GHC -Wall #-}
{-# Language GADTs #-}
{-# Language FlexibleContexts #-}
{-# Language TypeOperators #-}
{-# Language TypeFamilies #-}

module Dvda.HSBuilder ( buildHSFunction
                      , buildHSFunctionPure
                      , buildHSFunctionFromGraph
                      ) where

import qualified Data.Hashable as H
import System.Directory
import Control.Monad(when)
import qualified System.Plugins.Make as Make
import qualified System.Plugins.Load as Load
import Numeric.LinearAlgebra ( Element )

import Dvda.HSSyntax ( writeHSSource )
import Dvda.Graph ( FunGraph(..) )
import Dvda.SymMonad ( MkIO(..), makeFunGraph )
import qualified Dvda.Config as Config


-- | take in a pure function and symbolic inputs, return JIT compiled function
buildHSFunctionPure :: (Show (NumT c), Element (NumT c), H.Hashable (NumT c), MkIO c,
                        MkIO b, NumT b ~ NumT c) =>
                       (b -> c) -> b -> IO (GenT b -> GenT c)
buildHSFunctionPure fg xs = buildHSFunction xs (fg xs)


-- | take in symbolic inputs and outputs, return JIT compiled function
buildHSFunction :: (Show (NumT b), Element (NumT b), H.Hashable (NumT b), MkIO b,
                    MkIO c, NumT c ~ NumT b) =>
                   b -> c -> IO (GenT b -> GenT c)
buildHSFunction inputs outputs = buildHSFunctionFromGraph $ makeFunGraph inputs outputs


-- | take in FunGraph, return JIT compiled function
buildHSFunctionFromGraph :: (Show a, Element a, H.Hashable a, MkIO c, MkIO b) =>
                            FunGraph a b c -> IO (GenT b -> GenT c)
buildHSFunctionFromGraph fg = do
  -- source and hash
  let hash = show $ abs $ H.hash fg
      source = writeHSSource fg hash 

  -- function directory
  dir <- Config.functionDir hash
  
  -- make function directory if it doesn't exist
  createDirectoryIfMissing False dir
  
  -- filenames
  let sourcePath  = dir ++ "/" ++ Config.nameHSSource  hash
      -- objectPath  = dir ++ "/" ++ Config.nameHSObject  hash
      
  -- if the source already exists, make sure it matches the old source
  srcExists <- doesFileExist sourcePath
  when srcExists $ do
    oldSrc <- readFile sourcePath
    when (source /= oldSrc) $ putStrLn $
      "====================================================\n" ++ 
      "WARNING: Hash not unique or source code has been edited\n"++ 
      "If you have not edited the auto-generated code, please let me\n" ++
      "know that Hash collisions are a problem at gregmainland@gmail.com\n" ++
      "====================================================\n\n"
  
  -- write  source
  putStrLn "writing source"
  writeFile sourcePath source

  -- compile code
  putStrLn "building source"
  objpath <- makeWithPlugins sourcePath
--  objpath <- makeWithProcess sourcePath objectPath
  
  -- load object
  putStrLn "loading object"
  loadWithPlugins objpath hash


makeWithPlugins :: FilePath -> IO FilePath
makeWithPlugins sourcePath = do 
  status <- Make.make sourcePath [] -- ["-v3"]
  
  case status of (Make.MakeSuccess _ path) -> do putStrLn "Success!"
                                                 return path
                 (Make.MakeFailure code)   -> do mapM_ putStrLn code
                                                 error "Make Failure"
  

loadWithPlugins :: FilePath -> String -> IO a
loadWithPlugins objpath hash = do
  status' <- Load.load_ objpath [] (Config.nameHSFunction hash)
  case status' of (Load.LoadFailure codes) -> do mapM_ putStrLn codes
                                                 error "Load Failure"
                  (Load.LoadSuccess _ fun) -> do putStrLn "load success!"
                                                 return fun
