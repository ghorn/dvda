{-# OPTIONS_GHC -Wall #-}
{-# Language GADTs #-}
{-# Language FlexibleContexts #-}
{-# Language TypeOperators #-}
{-# Language TypeFamilies #-}
{-# Language ForeignFunctionInterface #-}

module Dvda.Codegen.Utils ( callGcc
                          , callGhc
                          ) where

import System.Process(runCommand, waitForProcess)
import System.Exit(ExitCode(ExitSuccess))
import Control.Monad(when)

import qualified Dvda.Config as Config

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


-- | take in name of source and future object, compile object
callGhc :: FilePath -> FilePath -> IO ()
callGhc srcname objname = do
  -- compile new object
  let compileString = Config.ghcString srcname objname
      displayString = Config.ghcString (shortName srcname) (shortName objname)

  -- print compilation string
  when Config.spewGccCall $ putStrLn displayString
  
  -- run compilation string
  p <- runCommand compileString
  
  -- check for errors
  exitCode <- waitForProcess p
  when (exitCode /= ExitSuccess) $ error $ "failed compiling " ++ srcname


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


