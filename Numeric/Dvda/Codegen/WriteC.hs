-- WriteC.hs

{-# OPTIONS_GHC -Wall #-}

module Numeric.Dvda.Codegen.WriteC( writeCSource
                                  , showCSource
                                  , showCInclude
                                  ) where

import Data.Hash.MD5(md5s, Str(..))
import Data.Tuple.Utils(fst3,snd3)

import qualified Numeric.Dvda.Config as Config
import Numeric.Dvda.Internal.ExprUtils
import Numeric.Dvda.Internal.ExprGraph
import Numeric.Dvda.Internal.GNode(topSort)
import Numeric.Dvda.Internal.Expr

-- | show c source utility function
showCSource :: (Eq a, Show a) => [Expr a] -> [Expr a] -> String
showCSource inputs outputs = fst3 $ writeCSource inputs outputs
      
-- | show c include utility function
showCInclude :: (Eq a, Show a) => [Expr a] -> [Expr a] -> String
showCInclude inputs outputs = snd3 $ writeCSource inputs outputs

writeCSource :: (Eq a, Show a) => [Expr a] -> [Expr a] -> (String, String, String)
writeCSource inputs outputs = (src, include, hash)
  where
    hash = md5s (Str body)
    
    prototype = "void " ++ Config.nameCFunction hash ++ "(const double in[], double out[])"
    
    include = unlines [ "// " ++ Config.nameCInclude hash
                      , ""
                      , "#ifndef __" ++ hash ++ "__"
                      , "#define __" ++ hash ++ "__"
                      , ""
                      , prototype ++ ";"
                      , ""
                      , "#endif //__" ++ hash ++ "__"
                      ]

    src = unlines [ "// " ++ Config.nameCSource hash
                  , ""
                  , "#include \"math.h\""
                  , "#include \"" ++ Config.nameCInclude hash ++ "\""
                  , ""
                  , prototype 
                  , "{"
                  , body ++ "}"
                  ]

    body = "    // input declarations:\n" ++ 
           unlines inputDeclarations ++
           "\n    // body:\n" ++
           unlines (map (\x -> "    " ++ toCCode x) (topSort gnodes)) ++
           "\n    // output declarations:\n" ++
           unlines outputDeclarations
      where
        inputDeclarations = zipWith (\x k -> "    double " ++ show x ++" = in["++show k++"];")
                            inputs [(0::Integer)..]
        outputDeclarations = zipWith (\oi k -> "    out[" ++ show k ++ "] = " ++ Config.cName oi ++ ";")
                             outputIndices [(0::Integer)..]
        (gnodes, outputIndices) = exprsToGNodes outputs
