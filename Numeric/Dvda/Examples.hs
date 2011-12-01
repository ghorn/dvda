-- Examples.hs

{-# OPTIONS_GHC -Wall #-}

module Numeric.Dvda.Examples( simpleGraph
                            , tensorGraph
--                            , fadExample
--                            , radExample
                            , codegenExample
                            ) where


import Numeric.Dvda.Expr
import Numeric.Dvda.Vis
--import Numeric.Dvda.AD.Fad(fad)
--import Numeric.Dvda.AD.Rad(rad)
import Numeric.Dvda.Function

simpleGraph :: IO ()
simpleGraph = do
  let x = sym "x" :: Expr Double
      y = x*2 + cos(x)/5
  previewExprs [y, x*x]

tensorGraph :: IO ()
tensorGraph = do
  let x = symMat (3,5) "x"
      y = sym "y"
      z = x*2 + cos(x)/5 + y
  previewExprs [z :: Expr Double]

--fadExample :: IO ()
--fadExample = do
--  let f x = [x*34 + 5, x*34 + 4/x, sin x]
--      y = symbolic "y" :: Expr Double
--      expr = f y
--      g = map simplify $ fad f y
--
--  print expr
--  print $ expr
--  print $ g
--  previewGraph $ exprsToGraph expr
--  previewGraph $ exprsToGraph g
--  
--
--radExample :: IO ()
--radExample = do
--  let exampleExpr :: Expr Double
--      exampleExpr = (z + x*y)*log(cos(x)/(tanh(y)))**(z/exp(y))
--      x = symbolic "x"
--      y = symbolic "y"
--      z = symbolic "z"
--      args = [x,y,z]
--
--  print exampleExpr
--  print $ rad exampleExpr args
--  
--  previewGraph_ $ lexprToGraph ("f", exampleExpr)
--  previewGraph_ $ lexprsToGraph $ zip ["f", "df/dx", "df/dy", "df/dz"] (exampleExpr:(rad exampleExpr args))


codegenExample :: IO ()
codegenExample = do

  let f :: Floating a => [a] -> [a]
      f [x',y'] = [y'/cos(x'), 23423*atan(100*x')]
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
