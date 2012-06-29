-- Config.hs

{-# OPTIONS_GHC -Wall #-}

module Dvda.Config( -- * directory stuff
                    dvdaDir
                    -- * C syntax
                  , cType
                  , cName
                  , nameCSource
                  , nameCInclude
                  , nameCObject
                  , nameCFunction
                    -- * Haskell syntax
                  , nameHSObject
                  , nameHSModule
                  , nameHSFunction
                  , nameHSSource
                  , nameHSVar
                  , nameHSConst
                    -- * Octave
                  , nameOctaveSource
                    -- * gcc stuff
                  , gccString
                  , spewGccCall
                  , outputNames
                    -- * ghc stuff
                  , ghcString
                    -- * symbolic stuff
                  , simplifyCommutativeOps
                  ) where

import System.Directory
import Control.Monad(unless)

-- | what symbolic variable names to use when generating a function
outputNames :: [String]
outputNames = map (\x -> "out"++show x) [(0::Integer)..]

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

ghcString :: FilePath -> FilePath -> String
ghcString src obj = "ghc -c " ++ src ++ " -o " ++ obj


-- c syntax
-- | type to use when generating c code
cType :: String
cType = "double"

-- | name convention for c variables
cName :: Int -> String
cName k
  | k < 0 = error "cName got negative index" 
  | otherwise = 't':show k     

nameCSource :: String -> String
nameCSource hash = nameCFunction hash ++ ".c"

nameCInclude :: String -> String
nameCInclude hash = nameCFunction hash ++ ".h"

nameCObject :: String -> String
nameCObject hash = "c_" ++ nameCFunction hash ++ ".o"

nameCFunction :: String -> String
nameCFunction hash = "call_" ++ hash

-- haskell syntax
nameHSVar :: Int -> String
nameHSVar k
  | k < 0 = error "nameHSVar got negative index" 
  | otherwise = 'v':show k

-- haskell syntax
nameHSConst :: Int -> String
nameHSConst k
  | k < 0 = error "nameHSConst got negative index" 
  | otherwise = 'c':show k

nameHSFunction :: String -> String
nameHSFunction hash = "call_" ++ hash

nameHSModule :: String -> String
nameHSModule hash = "Call_" ++ hash

nameHSSource :: String -> String
nameHSSource = (++ ".hs") . nameHSModule

nameOctaveSource :: String -> String
nameOctaveSource = (++ ".m")

nameHSObject :: String -> String
nameHSObject = (++ ".o") . nameHSModule

-- | whether e.g. x + y == y + x or not
simplifyCommutativeOps :: Bool
simplifyCommutativeOps = True

