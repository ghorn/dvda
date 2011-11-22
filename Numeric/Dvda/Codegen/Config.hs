-- Config.hs

{-# OPTIONS_GHC -Wall #-}

module Numeric.Dvda.Codegen.Config( cType
                                  , dvdaDir
                                  , functionDir
                                  , nameCSource
                                  , nameCInclude
                                  , nameCObject
                                  , nameCFunction
                                  , gccString
                                  ) where

import System.Directory
import Control.Monad(when)

cType :: String
cType = "double"

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


-- take in source file and object, return string suitible for calling to compile
gccString :: FilePath -> FilePath -> String
gccString src obj = "gcc -O2 -fPIC -shared " ++ src ++ " -o " ++ obj


functionDir :: String -> IO FilePath
functionDir hash = do
  -- dvda directory
  topDir <- dvdaDir
  return (topDir ++ "/" ++ hash)


nameCSource :: String -> String
nameCSource hash = nameCFunction hash ++ ".c"

nameCInclude :: String -> String
nameCInclude hash = nameCFunction hash ++ ".h"

nameCObject :: String -> String
nameCObject hash = nameCFunction hash ++ ".o"

nameCFunction :: String -> String
nameCFunction hash = "call_" ++ hash

