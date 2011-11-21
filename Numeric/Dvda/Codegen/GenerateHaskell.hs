-- GenerateHaskell.hs

{-# OPTIONS_GHC -Wall #-}

module Numeric.Dvda.Codegen.GenerateHaskell( toHaskellSource
                                           ) where

toHaskellSource :: String -> String
toHaskellSource hash = header
  where
    name = "HS_" ++ hash
    header = unlines [ "-- " ++ name ++ ".hs"
                     , ""
                     , "{-# OPTIONS_GHC -Wall #-}"
                     , "{-# LANGUAGE ForeignFunctionInterface #-}"
                     , ""
                     , "module " ++ name ++ "(c_call) where"
                     , ""
                     , "import Foreign.C.Types(CDouble)"
                     , "import Foreign.Ptr(Ptr)"
                     , ""
                     , "foreign import ccall unsafe \"" ++ hash ++ ".h call\" c_call :: Ptr CDouble -> Ptr CDouble -> IO ()"
                     ]
