-- Config.hs

{-# OPTIONS_GHC -Wall #-}

module Dvda.Config( cType
                  , cName
                  , dvdaDir
                  , functionDir
                  , nameCSource
                  , nameCInclude
                  , nameCObject
                  , nameCFunction
                  , gccString
                  , spewGccCall
                  , outputNames
                  ) where

import System.Directory
import Control.Monad(unless)

-- | what symbolic variable names to use when generating a function
outputNames :: [String]
outputNames = map (\x -> "out"++show x) [(0::Integer)..]

-- | type to use when generating c code
cType :: String
cType = "double"

-- | name convention for c variables
cName :: Int -> String
cName idx = 't':show idx

-- | whether to print the gcc call when generating code
spewGccCall :: Bool
spewGccCall = True

-- | return directory to use for temp files
-- | create this directory and print message if it doesn't exist
dvdaDir :: IO FilePath
dvdaDir = do
  dir <- getAppUserDataDirectory "dvda"
  
  -- print message if creating directory
  exist <- doesDirectoryExist dir
  unless exist $ putStrLn $ "creating directory \""++dir++"\" for codegen source/objects"

  -- make the directory if missing
  createDirectoryIfMissing True dir
  
  return dir


-- | take in source file and object, return string suitible for calling to compile
gccString :: FilePath -> FilePath -> String
gccString src obj = "gcc -O2 -std=gnu99 -fPIC -shared -Wall -Wextra -Werror " ++ src ++ " -o " ++ obj


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
nameCFunction hash = hash
