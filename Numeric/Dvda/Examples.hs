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
                            , showCCodeExample
                            ) where

import Numeric.Dvda

-- | Make a graph of a simple equation
simpleGraph :: IO ()
simpleGraph = do
  let x = sym "x" :: Expr Double
      y = x*2 + cos x / 5
  previewExprs [y, x*x]

-- | Make a graph of a simple equation involving matrices
tensorGraph :: IO ()
tensorGraph = do
  let x = symMat (3,5) "x"
      y = sym "y"
      z = x*2 + cos x / 5 + y
  previewExprs [z :: Expr Double]

-- | Make a graph of forward-mode AD
fadExample :: IO ()
fadExample = do
  let f x = [x*34 + 5, x*34 + 4/x, sin x]
      y = sym "y" :: Expr Double
      exprs = f y
      g = fad f y

  print exprs
  print g
  previewExprs exprs
  previewExprs g
  
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
  
  previewExprs [exampleExpr]
  previewExprs_ $ exampleExpr:rad exampleExpr args
--  previewExprs lexprToGraph ("f", exampleExpr)
--  previewExprs lexprsToGraph $ zip ["f", "df/dx", "df/dy", "df/dz"] (exampleExpr:(rad exampleExpr args))


-- | Turn expressions into a function and call it natively and through the auto-generated C code
codegenExample :: IO ()
codegenExample = do

  let f :: Floating a => [a] -> [a]
      f [x',y'] = [y'/cos x', 23423*atan(100*x')]
      f _ = error "bad testFun inputs"

      x = sym "x"
      y = sym "y"
      
  -- make function
  fun <- toFunction [x,y] (f [x,y])
  
  -- call different ways
  putStr "call f directly:              "
  print $ f [12,13::Double]
  
  putStr "callC Function:               "
  print $ callC fun [12,13::Double]
  
  putStr "callNative Function:          "
  print $ callNative fun [12,13 :: Expr Double]


-- | show some c code
showCCodeExample :: IO ()
showCCodeExample = do
  let x = sym "x" :: Expr Double
      y = sym "y"
      z = sym "z"
      f = (z + x*y)*log(cos x / tanh y)
      df = rad f [x,y,z]
  putStrLn "function:"
  print f
  putStrLn "\ngradients:"
  mapM_ print df
  putStrLn "\nC source:"
  putStrLn $ showCSource [x,y,z] (f:df)
