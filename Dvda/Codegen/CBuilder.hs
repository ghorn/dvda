{-# OPTIONS_GHC -Wall #-}
{-# Language GADTs #-}
{-# Language FlexibleContexts #-}
{-# Language TypeOperators #-}
{-# Language TypeFamilies #-}
{-# Language ForeignFunctionInterface #-}

module Dvda.Codegen.CBuilder ( buildCFunction
                             ) where

import qualified Data.Hashable as H
import qualified Data.HashMap.Lazy as HM
import qualified Data.Vector.Unboxed as V

import System.Directory
import System.Process(runCommand, waitForProcess)
import System.Exit(ExitCode(ExitSuccess))
import Control.Monad(when)

import qualified Dvda.Config as Config
import Dvda.Graph
import Dvda.Codegen.CSyntax ( writeCSource, writeCInclude )

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
