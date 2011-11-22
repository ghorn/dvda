-- Codegen.hs

{-# OPTIONS_GHC -Wall #-}

module Numeric.Dvda.Codegen.Codegen( toFunction
                                   ) where

import System.Directory
import System.Posix.DynamicLinker
import System.Process(runCommand, waitForProcess)
import System.Exit(ExitCode(ExitSuccess))
import Control.Monad(when)

import qualified Numeric.Dvda.Codegen.Config as Config
import Numeric.Dvda.Function
import Numeric.Dvda.Codegen.GenerateC(generateCSource)
import Numeric.Dvda.Expr.Expr(Expr(..))


-- shorten path name for display purposes
shortName :: String -> String
shortName full
  | length name <= maxN = name ++ extension
  | otherwise           = (take firstN name) ++ "..." ++ (drop (length name - lastN) name) ++ extension
  where
    firstN = 20
    lastN  = 10
    maxN = firstN + lastN

    (name, extension) = break (== '.') $ reverse $ takeWhile (/= '/') (reverse full)


-- take in name of source and future object, compile object
callGcc :: FilePath -> FilePath -> IO ()
callGcc srcname objname = do
  -- compile new object
  let compileString = Config.gccString srcname objname
      displayString = Config.gccString (shortName srcname) (shortName objname)

  -- print compilation string
  putStrLn displayString
  
  -- run compilation string
  p <- runCommand compileString
  
  -- check for errors
  exitCode <- waitForProcess p
  when (exitCode /= ExitSuccess) $ error $ "failed compiling " ++ srcname


-- make source functions
buildCFunction :: (Eq a, Show a) => [Expr a] -> [Expr a] -> IO (String, FilePath)
buildCFunction inputs outputs = do
  -- C source and hash
  let (cSource, cInclude, srcHash) = generateCSource inputs outputs
      
  -- function directory
  dir <- Config.functionDir srcHash
  
  -- make function directory if it doesn't exist
  createDirectoryIfMissing False dir
  
  -- filenames
  let cSourceFile  = dir ++ "/" ++ Config.nameCSource  srcHash
      cIncludeFile = dir ++ "/" ++ Config.nameCInclude srcHash
      cObjectFile  = dir ++ "/" ++ Config.nameCObject  srcHash
      
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

  -- compile code
  callGcc cSourceFile cObjectFile
  
  -- return info
  return (srcHash, cObjectFile)
  

toFunction :: RealFrac a => [Expr a] -> [Expr a] -> IO (Function a)
toFunction inputs outputs = do
  -- TODO: make sure all inputs are accounted for - issue 19
  (hash, objPath) <- buildCFunction inputs outputs

  dl <- dlopen objPath []
  funptr <- dlsym dl $ Config.nameCFunction hash

  return $ Function { funInputs  = inputs
                    , funOutputs = outputs
                    , funHash    = hash
                    , funCFunPtr = funptr
                    }
