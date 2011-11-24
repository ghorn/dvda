-- RunTests.hs

{-# OPTIONS_GHC -Wall #-}

module Main where
  
import Numeric.Dvda.Tests.Tests( runTests )
                               
main :: IO ()
main = runTests
