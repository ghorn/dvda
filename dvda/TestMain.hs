{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}

module Main where

import Test.Framework ( defaultMain ) 

import Dvda.Tests.Unary ( unaryTests )

-- Arbitrary numerical functions
--binary :: Floating a => [a -> a -> a]
--binary = [(*), (+), (-), (/)]
--
--unary :: Floating a => [a -> a]
--unary = [abs, negate, signum, exp, sqrt, log, sin, cos, tan, asin, acos, atan, tanh, sinh, cosh, atanh, asinh, acosh]
main :: IO ()
main = defaultMain [unaryTests]
