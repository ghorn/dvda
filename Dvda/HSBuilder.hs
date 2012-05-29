{-# OPTIONS_GHC -Wall #-}
{-# Language GADTs #-}
{-# Language FlexibleContexts #-}
{-# Language TypeOperators #-}
{-# Language TypeFamilies #-}

module Dvda.HSBuilder ( buildHSFunction
                      , buildHSFunctionFromGraph
                      ) where

import qualified Data.Hashable as H
import System.Directory
import Control.Monad(when)
import qualified System.Plugins.Make as Make
import qualified System.Plugins.Load as Load
import Numeric.LinearAlgebra ( Element )
--import System.Process( runCommand, waitForProcess )
--import System.Exit( ExitCode(ExitSuccess) )

import Dvda.HSSyntax ( writeHSSource )
import Dvda.Graph ( FunGraph(..) )
import Dvda.SymMonad ( Exprs, HList(..), makeFunGraph )
import qualified Dvda.Config as Config


-- | make source functions
buildHSFunction :: (Show (DimT b), Show (DimT c), Show (NumT b), Element (NumT b), H.Hashable (NumT b),
                    HList b, HList c,  NumT c ~ NumT b) =>
                   (b -> c) -> b -> IO (Exprs (DimT b) Double -> Exprs (DimT c) Double)
buildHSFunction fg xs = buildHSFunctionFromGraph $ makeFunGraph xs (fg xs)


-- | make source functions
buildHSFunctionFromGraph :: (H.Hashable a, Show a, Element a, Show b, Show c) =>
                            FunGraph a b c -> IO (Exprs b Double -> Exprs c Double)
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

-- -- | take in name of source and future object, compile object
-- makeWithProcess :: FilePath -> FilePath -> IO FilePath
-- makeWithProcess srcname objname = do
--   -- compile new object
--   let compileString = Config.ghcString srcname objname
--       displayString = Config.ghcString (shortName srcname) (shortName objname)
-- 
--   -- print compilation string
--   when Config.spewGccCall $ putStrLn displayString
--   
--   -- run compilation string
--   p <- runCommand compileString
--   
--   -- check for errors
--   exitCode <- waitForProcess p
--   when (exitCode /= ExitSuccess) $ error $ "failed compiling " ++ srcname
--   
--   return objname
-- 
-- 
-- -- | shorten path name for display purposes
-- shortName :: String -> String
-- shortName full
--   | length name <= maxN = name ++ extension
--   | otherwise           = take firstN name ++ "..." ++ drop (length name - lastN) name ++ extension
--   where
--     firstN = 20
--     lastN  = 10
--     maxN = firstN + lastN
-- 
--     (name, extension) = break (== '.') $ reverse $ takeWhile (/= '/') (reverse full)
