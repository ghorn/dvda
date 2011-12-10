{- |
   Module      : Numeric.Dvda.Examples
   Description : Simple examples to get you started.

   Find the source for these examples if you're having trouble figuring out how to use this library.
 -}

{-# OPTIONS_GHC -Wall #-}

module Numeric.Dvda.Examples( simpleGraph
                            , tensorGraph
                            , fadExample
                            , radExample
                            , codegenExample
                            , codegenExampleV
                            , showCCodeExample
                            ) where

import Numeric.Dvda

-- | Make a graph of a simple equation
simpleGraph :: IO ()
simpleGraph = do
  let x = sym "x" :: Expr Double
      y = x*2 + cos x / 5
  previewExprs [y, x*x] ["f0", "f1"]

-- | Make a graph of a simple equation involving matrices
tensorGraph :: IO ()
tensorGraph = do
  let x = symMat (3,5) "x"
      y = sym "y"
      z = x*2 + cos x / 5 + y
  previewExprs [z :: Expr Double] ["z"]

-- | Make a graph of forward-mode AD
fadExample :: IO ()
fadExample = do
  let f x = [x*34 + 5, x*34 + 4/x, sin x]
      y = sym "y" :: Expr Double
      exprs = f y
      g = fad f y

  print exprs
  print g
  previewExprs exprs ["f0","f1","f2"]
  previewExprs g ["g0","g1","g2"]
  
-- | Make a graph of reverse-mode AD
radExample :: IO ()
radExample = do
  let exampleExpr :: Expr Double
      exampleExpr = (z + x*y)*log(cos x / tanh y)**(z/exp y)
      
      x = sym "x"
      y = sym "y"
      z = sym "z"
      args = [x,y,z]

  print exampleExpr
  print $ rad exampleExpr args
  
  previewExprs [exampleExpr] ["f"]
  previewExprs_ (exampleExpr:(rad exampleExpr args)) ["f","df/dx","df/dy","df/dz"]

-- | Turn vector expressions into a function and call it natively and through the auto-generated C code
codegenExampleV :: IO ()
codegenExampleV = do

  let f :: Floating a => [a] -> [a]
      f [x,y] = [y/cos x, 23423*atan(100*x)]
      f _ = error "bad testFun inputs"

      xs = sym "x"
      ys = symVec 3 "y"
      
      x' = 2.52 :: Double
      y' = [1,5,pi] :: [Double]
      
  print $ eval $ vec y' / cos( sca x' )
  -- make function
  fun <- toFunction [xs,ys] $ f [xs,ys]
  
  putStr "callC Function:      "
  print $ callC fun [sca x',vec y']

  putStr "callNative Function: "
  print $ callNative fun [sca x',vec y']


-- | Turn scalar expressions into a function and call it natively and through the auto-generated C code
codegenExample :: IO ()
codegenExample = do

  let f :: Floating a => [a] -> [a]
      f [x,y] = [y/cos x, 23423*atan(100*x)]
      f _ = error "bad testFun inputs"

      xs = sym "x"
      ys = sym "y"
      
      x' = 2.52 :: Expr Double
      y' = 21.0
      
  -- make function
  fun <- toFunction [xs,ys] $ f [xs,ys]
  
  putStr "callC Function:      "
  print $ callC fun [x', y']

  putStr "callNative Function: "
  print $ callNative fun [x',y']


-- | show some c code
showCCodeExample :: IO ()
showCCodeExample = do
  let x = sym "x" :: Expr Double
      y = symVec 3 "y"
      z = sym "z"
      f = cos $ ((vec [1,2,1]) + z + x*y)*log(cos x / tanh y)
      df = rad f [x,y,z]
  putStrLn "function:"
  print f
  putStrLn "\ngradients:"
  mapM_ print df
  putStrLn "\nC source:"
  putStrLn $ showCSource [x,y,z] (f:df)
