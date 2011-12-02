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
import Numeric.Dvda.Internal.Tensor

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
    
    prototype = "void " ++ Config.nameCFunction hash ++ "(const double * const in[], double * const out[])"
    
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
                  , "#include \"string.h\""
                  , "#include \"" ++ Config.nameCInclude hash ++ "\""
                  , ""
                  , prototype 
                  , "{"
                  , body ++ "}"
                  ]

    body = "    // input declarations:\n" ++ 
           unlines inputDeclarations ++
           "\n    // body:\n" ++
           unlines (map (\x -> "    " ++ toCCode x) (topSort gnodes))
      where
        inputDeclarations = zipWith inputDec inputs [0..]
        gnodes = exprsToGNodes outputs
        
inputDec :: Expr a -> Int -> String
inputDec (EScalar (TSym _ n)) k = "    const double " ++ n ++ " = *(in[" ++ show k ++ "]);"
inputDec (EVector (TSym _ n)) k = "    const double * const " ++ n ++ " = in[" ++ show k ++ "];"
inputDec (EMatrix (TSym _ n)) k = "    const double * const " ++ n ++ " = in[" ++ show k ++ "];"
inputDec _ _ = error "api fail in inputDec"
