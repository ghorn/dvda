{-# OPTIONS_GHC -Wall #-}

module Dvda.ShowExprTests ( runTests
                          ) where

import Data.Maybe ( mapMaybe )

import Dvda.Expr

someShows :: [(String, Expr Double)]
someShows = [ ("x * y", x * y)
            , ("x / y", x / y)
            , ("(x * y) * z", x * y * z)
            , ("(x * y) / z", x * y / z)
            , ("x / (y * z)", x / (y * z))
            , ("cos(x)", cos x)
            , ("sin(cos(x))", sin (cos x))
            , ("sin(x ** y)", sin (x ** y))
            , ("sin(x + y)", sin (x + y))
            , ("x ** sin(y)", x ** sin y)
            , ("(x + y) * z", (x + y)*z)
            , ("10 * x", 10*x)
            ]
  where
    x = sym "x"
    y = sym "y"
    z = sym "z"

testShows :: [(String, Expr Double)] -> IO ()
testShows = putStrLn . unlines . map betterShow . mapMaybe testShow
  where
    betterShow (x,y) = x ++ " =/= " ++ y
    testShow (str,expr)
      | expr' == str = Nothing
      | otherwise = Just (expr', str)
      where
        expr' = show expr

runTests :: IO ()
runTests = testShows someShows
