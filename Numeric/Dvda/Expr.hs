-- Expr.hs

{-# OPTIONS_GHC -Wall #-}

module Numeric.Dvda.Expr( module Numeric.Dvda.Expr.Expr
                        , module Numeric.Dvda.Expr.Binary
                        , module Numeric.Dvda.Expr.Unary
                        , module Numeric.Dvda.Expr.Properties
                        ) where

-- Scalar/Vector/Matrix are used only under the hood. If a user tries to use these the API will break
import Numeric.Dvda.Expr.Expr hiding (Scalar(..), Vector(..), Matrix(..))
import Numeric.Dvda.Expr.Binary
import Numeric.Dvda.Expr.Unary
import Numeric.Dvda.Expr.Properties
