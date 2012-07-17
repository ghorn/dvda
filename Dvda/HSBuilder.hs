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

import Dvda.Codegen ( writeSourceFile )
import Dvda.HSSyntax ( GenHaskell, writeHSSource )
import Dvda.Graph ( FunGraph(..) )
import Dvda.SymMonad ( MkFunGraph(..), makeFunGraph )
import qualified Dvda.Config as Config


-- | take in a pure function and symbolic inputs, return JIT compiled function
buildHSFunctionPure :: (Show (NumT c), H.Hashable (NumT c), MkFunGraph c,
                        MkFunGraph b, NumT b ~ NumT c, GenHaskell b, GenHaskell c) =>
                       (b -> c) -> b -> IO (GenT b -> GenT c)
buildHSFunctionPure fg xs = buildHSFunction xs (fg xs)


-- | take in symbolic inputs and outputs, return JIT compiled function
buildHSFunction :: (Show (NumT b), H.Hashable (NumT b), MkFunGraph b,
                    MkFunGraph c, NumT c ~ NumT b, GenHaskell b, GenHaskell c) =>
                   b -> c -> IO (GenT b -> GenT c)
buildHSFunction inputs outputs = buildHSFunctionFromGraph $ makeFunGraph inputs outputs


-- | take in FunGraph, return JIT compiled function
buildHSFunctionFromGraph :: (Show a, H.Hashable a, GenHaskell b, GenHaskell c) =>
                            FunGraph a b c -> IO (GenT b -> GenT c)
buildHSFunctionFromGraph fg = do
  topDir <- dvdaDir
  -- source and hash
  let hash = show $ abs $ H.hash fg
      source = writeHSSource fg hash
      sourceName = Config.nameHSSource hash
      funDir = topDir ++ "/" ++ nameCFunction hash

  sourcePath <- writeSourceFile source funDir sourceName
  
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
