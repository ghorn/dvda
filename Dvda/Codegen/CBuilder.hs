{-# OPTIONS_GHC -Wall #-}
{-# Language GADTs #-}
{-# Language FlexibleContexts #-}
{-# Language TypeOperators #-}
{-# Language TypeFamilies #-}
{-# Language ForeignFunctionInterface #-}

module Dvda.Codegen.CBuilder ( buildCFunction
                             ) where

import qualified Data.Hashable as H
import qualified Data.Vector.Unboxed as V

import System.Directory
import Control.Monad(when)

import qualified Dvda.Config as Config
import Dvda.Graph
import Dvda.Codegen.CSyntax ( writeCSource, writeCInclude )
import Dvda.Codegen.Utils ( callGcc )

-- | make source functions
--buildCFunction :: (Eq a, Show a) => [Expr a] -> [Expr a] -> IO (String, FilePath)
buildCFunction :: (Show a, V.Unbox a, H.Hashable a) => FunGraph a b c -> IO (String, FilePath)
buildCFunction fg = do
  -- C source and hash
  let hash = show $ abs $ H.hash fg
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
