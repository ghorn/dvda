{-# OPTIONS_GHC -Wall #-}

module Dvda.Examples ( doCse
                     , showFg
                     , cgen
                     , pygen
                     , mexgen
                     ) where

import Dvda.Expr
import Dvda.FunGraph
import Dvda.Codegen.CGen
import Dvda.Codegen.PythonGen
import Dvda.Vis ( previewGraph )
import Dvda.CSE ( cse )
import Dvda.AD ( rad )

-- a random function to use in different examples
someFunGraph :: IO (FunGraph Double)
someFunGraph = toFunGraph inputs outputs
  where
    x = sym "x" :: Expr Double
    y = sym "y"
    z = sym "z"
    w = sym "w"
    w1 = sym "w1"
    w2 = sym "w2"
    w3 = sym "w3"
    f0 = x*y + z + w1 + w2
    f2 = f0 * w2/w3
    
    f1 = [f0/2, f0*y, w, 0.0, 0]
    boo = x

    inputs = boo :* [y]:*[[z]] :* [w3,w1,w2,w]
    outputs = f0:*f1:*f2:*[[f0*f0]]:*(rad f2 [x,y,z,w,w1,w2,w3])

-- | do cse on a fungraph and count nodes
doCse :: IO ()
doCse = do
  fg' <- someFunGraph
  putStrLn $ "fungraph has " ++ show (countNodes fg') ++ " nodes"
  let fg = cse fg'
  putStrLn $ "fungraph has " ++ show (countNodes fg) ++ " nodes after cse"

-- | show a fungraph
showFg :: IO ()
showFg = someFunGraph >>= previewGraph

-- | c code generation
cgen :: IO ()
cgen = fmap (showC RowMajor "foo") someFunGraph >>= putStrLn

-- | python code generation
pygen :: IO ()
pygen = fmap (showPy "foo") someFunGraph >>= putStrLn

-- | mex function generation
mexgen :: IO ()
mexgen = fmap (showMex "foo") someFunGraph >>= putStrLn
