-- WriteC.hs

{-# OPTIONS_GHC -Wall #-}

module Numeric.Dvda.Codegen.WriteC( writeCSource
                                  ) where

import Data.Hash.MD5(md5s, Str(..))

import qualified Numeric.Dvda.Config as Config
import Numeric.Dvda.Expr
import Numeric.Dvda.Internal.ExprGraph
import Numeric.Dvda.Internal.GNode(topSort)

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
