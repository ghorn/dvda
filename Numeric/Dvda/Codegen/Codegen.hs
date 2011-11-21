-- Codegen.hs

{-# OPTIONS_GHC -Wall #-}

module Numeric.Dvda.Codegen.Codegen( test
                                   ) where

import System.Directory
import System.Process(runCommand, waitForProcess)
import System.Exit(ExitCode(ExitSuccess))
import Data.Hash.MD5(md5s, Str(..))
import Control.Monad(when)
import Data.List(nub)
import Numeric.Dvda.Expr.Apply(getSyms)

import Numeric.Dvda.Function
import Numeric.Dvda.Codegen.GenerateC(functionToCSource)
import Numeric.Dvda.Codegen.GenerateHaskell
import Numeric.Dvda.Expr.Expr(Expr(..), symbolic)

test :: IO ()
test = do
--  putStrLn $ exprsToCFunction someExprs
  toFunction someExprs


someExprs :: [Expr Double]
someExprs = [x*y/cos(2), x*y]
  where
    x = symbolic "x"
    y = symbolic "y"


-- return directory to use for temp files
-- create this directory and print message if it doesn't exist
dvdaDir :: IO FilePath
dvdaDir = do
  dir <- getAppUserDataDirectory "dvda"
  
  -- print message if creating directory
  exist <- doesDirectoryExist dir
  when (not exist) $ putStrLn $ "creating directory \""++dir++"\" for codegen source/objects"

  -- make the directory if missing
  createDirectoryIfMissing True dir
  
  return dir


compileCCode :: FilePath -> FilePath -> IO ()
compileCCode srcname objname = do
  -- compile new object
  let compileString = "gcc -O2 -fPIC -shared " ++ srcname ++ " -o " ++ objname
  
  putStrLn compileString
  p <- runCommand compileString
  exitCode <- waitForProcess p
  when (exitCode /= ExitSuccess) $ error $ "failed compiling " ++ srcname


compileHaskellCode :: FilePath -> FilePath -> IO ()
compileHaskellCode srcname objname = do
  -- compile new object
  let compileString = "ghc --make -O2 -fPIC -shared " ++ srcname ++ " -o " ++ objname
  
  putStrLn compileString
  p <- runCommand compileString
  exitCode <- waitForProcess p
  when (exitCode /= ExitSuccess) $ error $ "failed compiling " ++ srcname


-- make source functions
writeSource :: (Eq a, Show a) => Function a -> IO (FilePath, String, (FilePath, FilePath), (FilePath, FilePath))
writeSource fun = do
  -- dvda directory
  topDir <- dvdaDir

  -- C source and filenames
  let (cSource, cInclude) = functionToCSource fun
      srcHash = md5s (Str cSource)
      dir = topDir ++ "/" ++ srcHash
      cSourceFile  = dir ++ "/" ++ srcHash ++ ".c"
      cIncludeFile = dir ++ "/" ++ srcHash ++ ".h"
      cObjectFile  = dir ++ "/" ++ srcHash ++ ".o"
      hsSourceFile = dir ++ "/HS_" ++ srcHash ++ ".hs"
      hsObjectFile = dir ++ "/HS_" ++ srcHash ++ ".o"
      
  -- make function directory if it doesn't exist
  createDirectoryIfMissing False dir
  
  -- if the source already exists, make sure it matches the old source
  srcExists <- doesFileExist cSourceFile
  when srcExists $ do
    oldSrc <- readFile cSourceFile
    when (cSource /= oldSrc) $ error "md5 sum not unique error, please let me know this actually happens at gregmainland@gmail.com"
  
  -- write c source
  writeFile cSourceFile cSource

  -- write c header
  writeFile cIncludeFile cInclude

  -- write haskell source
  writeFile hsSourceFile $ toHaskellSource srcHash

  -- return info
  return (dir, srcHash, (hsSourceFile, hsObjectFile), (cSourceFile, cObjectFile))

toFunction :: (Eq a, Show a) => [Expr a] -> IO ()
toFunction exprs = do
  let fun = Function { funInputs = nub $ concatMap getSyms exprs
                     , funOutputs = exprs
                     }
  (dir, hash, (hsrc, hobj), (csrc, cobj)) <- writeSource fun
  compileCCode csrc cobj
  compileHaskellCode hsrc hobj
