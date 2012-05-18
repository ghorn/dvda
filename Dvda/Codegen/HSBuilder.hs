{-# OPTIONS_GHC -Wall #-}
{-# Language GADTs #-}
{-# Language FlexibleContexts #-}
{-# Language TypeOperators #-}
{-# Language TypeFamilies #-}

module Dvda.Codegen.HSBuilder ( buildHSFunction
                              ) where

import qualified Data.Hashable as H
import qualified Data.HashMap.Lazy as HM
import System.Directory
import Control.Monad(when)
import System.Plugins.Make

import Dvda.Codegen.HSSyntax
import Dvda.Graph
import qualified Dvda.Config as Config


-- | make source functions
--buildHSFunction :: (Show a, Show b, Show c, V.Unbox a, H.Hashable a) => FunGraph a b c -> IO ()
buildHSFunction fg@(FunGraph hm _ _ _) = do
  -- C source and hash
  let hash = show $ abs $ H.hash $ HM.toList hm
      source = writeHSSource fg hash 

  -- function directory
  dir <- Config.functionDir hash
  
  -- make function directory if it doesn't exist
  createDirectoryIfMissing False dir
  
  -- filenames
  let sourcePath  = dir ++ "/" ++ Config.nameHSSource  hash
      objectPath  = dir ++ "/" ++ Config.nameHSObject  hash
      
  -- make function directory if it doesn't exist
  createDirectoryIfMissing False dir
  
  -- if the source already exists, make sure it matches the old source
  srcExists <- doesFileExist sourcePath
  when srcExists $ do
    oldSrc <- readFile sourcePath
    when (source /= oldSrc) $ putStrLn $
      "====================================================\n" ++ 
      "WARNING: Hash not unique or source code has been edited\n"++ 
      "If you have not edited the auto-generated code, please let me\n" ++
      "know that Hash clashes are a problem IRL at gregmainland@gmail.com\n" ++
      "====================================================\n\n"
  
  -- write  source
  putStrLn "writing source"
  writeFile sourcePath source

  -- compile code
  putStrLn "building source"
  status <- make sourcePath [] -- ["-v3"]
  
  case status of MakeSuccess code path -> do putStrLn "Success!"
                                             print code
                                             print path
                 MakeFailure code -> do putStrLn "Failure"
                                        mapM_ putStrLn code
  
  -- return info
--  return (hash, cObjectFile)
