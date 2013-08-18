{-# OPTIONS_GHC -Wall #-}

module Dvda.Codegen.Gcc ( compileWithGcc
                        ) where

import System.Process(runCommand, waitForProcess)
import System.Exit(ExitCode(ExitSuccess))
import Control.Monad(when)

-- | whether to print the gcc call when generating code
spewGccCall :: Bool
spewGccCall = True

-- | take in source file and object, return string suitible for calling to compile
gccString :: FilePath -> FilePath -> String
gccString src obj = "gcc -O2 -std=gnu99 -fPIC -shared -Wall -Wextra -Werror " ++ src ++ " -o " ++ obj

-- | take in name of source and future object, compile object
compileWithGcc :: FilePath -> FilePath -> IO ()
compileWithGcc srcname objname = do
  -- compile new object
  let compileString = gccString srcname objname

  -- print compilation string
  when spewGccCall $ putStrLn compileString
  
  -- run compilation string
  p <- runCommand compileString
  
  -- check for errors
  exitCode <- waitForProcess p
  when (exitCode /= ExitSuccess) $ error $ "failed compiling " ++ srcname
