{-# OPTIONS_GHC -Wall #-}

module Dvda.Examples ( run
                     , run1
                     , dotVis
                     ) where

import Dvda.Expr
import Dvda.FunGraph
import Dvda.CGen
import Dvda.Vis

run :: IO ()
run = do
  let x = sym "x" :: Expr Double
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
      outputs = f0:*f1:*f2:*[[f0*f0]]

--  showC "foo" inputs outputs >>= putStrLn

  fg' <- toFunGraph inputs outputs
  putStrLn $ "cost has " ++ show (countNodes fg') ++ " nodes"
  fg <- toFunGraph inputs outputs
  putStrLn $ "cost has " ++ show (countNodes fg) ++ " nodes"
  previewGraph fg
  putStrLn $ showMex "foo" fg

run1 :: IO ()
run1 = do
  let a = sym "a" :: Expr Double
      b = sym "b"
      c = sym "c"
      d = sym "d"
      e = sym "e"
      f = sym "f"

      inputs = [[a,b,c],[d,e,f]]
      outputs = [[a,b,c],[d,e,f]]

  fg <- toFunGraph inputs outputs
  putStrLn $ showC RowMajor "foo" fg
  putStrLn $ showMex "foo" fg

dotVis :: IO ()
dotVis = do
  let x = sym "x" :: Expr Double
      y = sym "y"
      z = sym "z"

      a0 = x + y + z
      a1 = x*a0/z
      a2 = a1 + a0/y

      inputs = x :* [y] :* [[z]]
      outputs = x*x :* a1 :* a0 :* [[a2]]
  fg <- toFunGraph inputs outputs
  previewGraph fg
