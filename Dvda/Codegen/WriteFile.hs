{-# OPTIONS_GHC -Wall #-}

module Dvda.Codegen.WriteFile ( writeSourceFile
                              ) where

import System.Directory
import Control.Monad ( when )

---- | return directory to use for temp files
---- | create this directory and print message if it doesn't exist
--dvdaDir :: IO FilePath
--dvdaDir = do
--  dir <- getAppUserDataDirectory "dvda"

writeSourceFile :: String -> FilePath -> FilePath -> IO FilePath
writeSourceFile source functionDir sourceName = do
  -- make function directory if it doesn't exist
  createDirectoryIfMissing False functionDir
  
  -- filenames
  let sourcePath  = functionDir ++ "/" ++ sourceName
      
  -- if the source already exists, make sure it matches the old source
  srcExists <- doesFileExist sourcePath
  when srcExists $
    putStrLn $ "file \"" ++ sourcePath ++ "\" already exists, overwriting"
  
  -- write  source
  putStrLn $ "writing " ++ sourcePath
  writeFile sourcePath source

  return sourcePath
