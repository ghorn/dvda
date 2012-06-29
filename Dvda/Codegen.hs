{-# OPTIONS_GHC -Wall #-}

module Dvda.Codegen ( writeSourceFile
                    ) where

import System.Directory
import Control.Monad(when)

import qualified Dvda.Config as Config

writeSourceFile :: String -> String -> FilePath -> IO FilePath
writeSourceFile hash source sourceName = do
  -- function directory
  dir <- Config.functionDir hash
  
  -- make function directory if it doesn't exist
  createDirectoryIfMissing False dir
  
  -- filenames
  let sourcePath  = dir ++ "/" ++ sourceName
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

  return sourcePath
