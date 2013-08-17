{-# OPTIONS_GHC -Wall #-}
{-# Language ForeignFunctionInterface #-}
{-# Language DeriveFunctor #-}
{-# Language DeriveFoldable #-}
{-# Language DeriveTraversable #-}

module Dvda.Llvm ( VList(..)
                 , withLlvmJit
                 , toLlvmAsm
                 ) where

import Control.Monad ( unless )
import Control.Monad.Trans.Error
import Data.Hashable ( Hashable )
import qualified Data.Foldable as F
import Data.Traversable ( Traversable )
import qualified Data.Vector as V
import Data.Word ( Word32 )
import Foreign.Ptr ( Ptr, FunPtr, castFunPtr )

import LLVM.General ( withModuleFromAST, moduleString )
import LLVM.General.Analysis ( verify )
import LLVM.General.Context ( withContext )
import LLVM.General.ExecutionEngine ( withJIT, withModuleInEngine, getFunction )
import LLVM.General.PassManager ( defaultCuratedPassSetSpec, runPassManager, withPassManager )

import qualified LLVM.General.AST as AST
import qualified LLVM.General.AST.Global as G
import qualified LLVM.General.AST.Constant as C
import LLVM.General.AST hiding ( Mul, Add, Sub )
import LLVM.General.AST.AddrSpace ( AddrSpace(..) )
import LLVM.General.AST.CallingConvention ( CallingConvention ( C ) )
import LLVM.General.AST.Constant ( Constant ( Float ) )
import LLVM.General.AST.Linkage ( Linkage ( External ) )
import LLVM.General.AST.Float (  SomeFloat ( Double ) )
import LLVM.General.AST.Visibility ( Visibility( Default ) )

import Dvda.Expr ( GExpr(..), Floatings(..), Nums(..), Fractionals(..) )
import Dvda.FunGraph ( FunGraph, fgTopSort, fgInputs, fgOutputs, fgLookupGExpr )
import Dvda.HashMap ( HashMap )
import qualified Dvda.HashMap as HM

doubleType :: Type
doubleType = FloatingPointType 64 IEEE

allIntrinsics :: [Definition]
allIntrinsics = [ unIntrinsic "exp"
                , unIntrinsic "log"
                , unIntrinsic "sin"
                , unIntrinsic "cos"
                , unIntrinsic "fabs"
                , binIntrinsic "pow"
                ]
  where
    intrinsic :: Type -> Int -> String -> Definition
    intrinsic typ nInputs name =
      GlobalDefinition
      Function { G.linkage = External
               , G.visibility = Default
               , G.callingConvention = C
               , G.returnAttributes = []
               , G.returnType = typ
               , G.name = Name $ "llvm."++name++".f64"
               , G.parameters = (replicate nInputs (Parameter typ (Name "") []),False)
               , G.functionAttributes = []
               , G.section = Nothing
               , G.alignment = 0
               , G.basicBlocks = []}

    binIntrinsic :: String -> Definition
    binIntrinsic = intrinsic doubleType 2

    unIntrinsic :: String -> Definition
    unIntrinsic = intrinsic doubleType 1

newtype VList a = VList {mkVList :: V.Vector (V.Vector a)} deriving (Functor, F.Foldable, Traversable)

toLlvmFun :: String -> FunGraph VList VList Double -> Global
toLlvmFun functionName fg = mainFun
  where
    mainFun = Function External Default C [] (IntegerType 32) (Name functionName)
              ( map toParameter $ ["input_"  ++ show k | k <- take nInputs  [(0::Int)..]] ++
                                  ["output_" ++ show k | k <- take nOutputs [(0::Int)..]]
              , False)
              [] Nothing 0 [bb]
    toParameter name = Parameter (PointerType doubleType (AddrSpace 0)) (Name name) []

    nInputs  = V.length $ mkVList (fgInputs  fg)
    nOutputs = V.length $ mkVList (fgOutputs fg)
    inputMap = makeInputMap $ mkVList (fgInputs  fg)
    outDecls = writeOutputs $ mkVList (fgOutputs fg)
    bb = BasicBlock (Name "entry") (instructions++outDecls)
         (Do $ Ret (Just (ConstantOperand (C.Int 32 0))) [])
    instructions = let f k = case fgLookupGExpr fg k of
                         Just v -> toInstruction inputMap k v
                         Nothing -> error $ "couldn't find node " ++ show k ++ " in fungraph :("
                   in concatMap f $ fgTopSort fg

--foreign import ccall "dynamic" mkIOStub ::
--  FunPtr (Ptr Double -> Ptr Double -> IO Word32) -> Ptr Double -> Ptr Double -> IO Word32

--runJIT :: ((V.Vector (Ptr Double) -> V.Vector (Ptr Double) -> IO Word32) -> IO a) -> AST.Module
--       -> IO (Either String a)
runJIT :: (FunPtr () -> IO a) -> Module -> IO (Either String a)
runJIT userFun mAST = do
  let --withEE = \c -> withMCJIT c Nothing Nothing Nothing Nothing
      withEE = flip withJIT 2
  withContext $ \context -> withEE context $ \executionEngine ->
    runErrorT $ withModuleFromAST context mAST $ \m ->
      withModuleInEngine executionEngine m $ \em -> do
        Just p <- getFunction em (Name "my_function")
--        let f = mkIOStub (castFunPtr p)
        userFun p

-- | you're on your own for making sure the FunPtr has the right number of inputs/outputs
--   until I can figure out a polyvariadic foreign import "dynamic"
withLlvmJit :: FunGraph VList VList Double -> (FunPtr () -> IO a) -> IO (Either String a)
withLlvmJit fg userfun = do
  let fundef = toLlvmFun "my_function" fg
      myModule = defaultModule { moduleName = "come_on_lets_compile"
                               , moduleDefinitions = allIntrinsics ++ [GlobalDefinition fundef]
                               }
  runJIT userfun myModule


toLlvmAsm :: FunGraph VList VList Double -> IO ()
toLlvmAsm fg = do
  let fundef = toLlvmFun "my_function" fg
      myModule = defaultModule { moduleName = "come_on_lets_compile"
                               , moduleDefinitions = allIntrinsics ++ [GlobalDefinition fundef]
                               }

  str <- withContext $ \context ->
    runErrorT $ withModuleFromAST context myModule $ \mdl -> do
      --putStrLn "------------------- IR: -----------------"
      --ms <- moduleString mdl
      --putStrLn ms
      let pss = defaultCuratedPassSetSpec
      ret <- withPassManager pss $ \pm -> runPassManager pm mdl
      unless ret (error "pass manager failed")
      vmdl <- runErrorT $ verify mdl
      case vmdl of Left err -> putStrLn ("ERROR:\n" ++ err) >> return Nothing
                   Right _ -> do
                     ms' <- moduleString mdl
                     return (Just ms')

  putStrLn $ case str of
    (Left x) -> x ++ "\n\n----invalid IR----"
    (Right Nothing) -> "invalid module"
    (Right (Just x)) -> x ++ "\n\n------- everything seems to have worked -----"


-- | take a list of pair of inputs to indices which reference them
--  create a hashmap from GSyms to strings which hold the declaration
makeInputMap :: (Eq a, Hashable a, Show a)
                => V.Vector (V.Vector (GExpr a Int)) -> HashMap (GExpr a Int) (Name,Word32)
makeInputMap ins = HM.fromList $ concat $ zipWith writeInput [(0::Int)..] (V.toList ins)
  where
    writeInput inputK gs = zipWith f [(0::Int)..] (V.toList gs)
      where
        f inIdx g = (g, (Name ("input_" ++ show inputK), fromIntegral inIdx))

writeOutputs :: V.Vector (V.Vector Int) -> [Named Instruction]
writeOutputs ins = concat $ zipWith outputInstructions [(0::Int)..] (V.toList ins)
  where
    outputInstructions outputK outputKvec =
      concat $ zipWith (outputInstruction outputK) [(0::Int)..] (V.toList outputKvec)
    outputInstruction outputK elemK node =
      [ nm :=
        GetElementPtr { inBounds = True
                      , address = LocalReference (Name ("output_"++show outputK))
                      , indices = [ConstantOperand (C.Int 64 (fromIntegral elemK))]
                      , metadata = []
                      }
      , Name "" :=
        Store { volatile = False
              , address = LocalReference nm
              , value = LocalReference (Name (nameNode node))
              , maybeAtomicity =  Nothing
              , alignment = 0
              , metadata = []
              }
      ]
      where
        nm = Name $ "tmp_output_" ++ show outputK ++ "_" ++ show elemK ++ "_" ++ show node

nameNode :: Int -> String
nameNode k = 'w' : show k

toInstruction :: HashMap (GExpr Double Int) (Name,Word32) ->
                 Int -> GExpr Double Int -> [Named Instruction]
toInstruction inputMap k gexpr = instruction gexpr
  where
    bin x y op =
      [Name (nameNode k) :=
       op (LocalReference (Name (nameNode x))) (LocalReference (Name (nameNode y))) []]

    makeDouble c =
      [Name (nameNode k) :=
       FAdd (ConstantOperand (Float (Double c))) (ConstantOperand (Float (Double 0))) []]

    instruction (GSym _) = case HM.lookup gexpr inputMap of
      Nothing -> error $ "toInstruction: couldn't find " ++ show gexpr ++ " in the input map"
      Just (inputK,inputElem) ->
        [ Name ("tmp_input_" ++ nameNode k) :=
          GetElementPtr { inBounds = True
                        , address = LocalReference inputK
                        , indices = [ConstantOperand (C.Int 64 (fromIntegral inputElem))]
                        , metadata = []
                        }
        , Name (nameNode k) :=
          Load { volatile = False
               , address = LocalReference (Name ("tmp_input_" ++ nameNode k))
               , maybeAtomicity = Nothing
               , alignment = 0
               , metadata = []
               }
        ]
    instruction (GConst c)                     = makeDouble c
    instruction (GNum (Mul x y))               = bin x y FMul
    instruction (GNum (Add x y))               = bin x y FAdd
    instruction (GNum (Sub x y))               = bin x y FSub
    instruction (GNum (Negate x))              = [Name (nameNode k) := FSub (ConstantOperand (Float (Double 0))) (LocalReference (Name (nameNode x))) []]
    instruction (GNum (FromInteger x))         = makeDouble (fromIntegral x)
    instruction (GFractional (Div x y))        = bin x y FDiv
    instruction (GFractional (FromRational x)) = makeDouble (fromRational x)

    instruction (GNum (Abs x))                = callUn x "fabs"
    instruction (GNum (Signum _x))             = error "pls implement sign"
    instruction (GFloating (Pow x y))          = callBin x y "pow"
    instruction (GFloating (LogBase _x _y))    = error "pls implement log( "
    instruction (GFloating (Exp x))            = callUn x "exp"
    instruction (GFloating (Log x))            = callUn x "log"
    instruction (GFloating (Sin x))            = callUn x "sin"
    instruction (GFloating (Cos x))            = callUn x "cos"
    instruction (GFloating (ASin _x))          = error "pls implement asin"
    instruction (GFloating (ATan _x))          = error "pls implement atan"
    instruction (GFloating (ACos _x))          = error "pls implement acos"
    instruction (GFloating (Sinh _x))          = error "pls implement sinh"
    instruction (GFloating (Cosh _x))          = error "pls implement cosh"
    instruction (GFloating (Tanh _x))          = error "pls implement tanh"
    instruction (GFloating (ASinh _))          = error "pls implement ASinh"
    instruction (GFloating (ATanh _))          = error "pls implement ATanh"
    instruction (GFloating (ACosh _))          = error "pls implement ACosh"

    callUn :: Int -> String -> [Named Instruction]
    callUn x name =
      [Name (nameNode k) := Call
      { isTailCall = False
      , callingConvention = C
      , returnAttributes = []
      , function = Right (ConstantOperand (C.GlobalReference (Name ("llvm."++name++".f64"))))
      , arguments = [(LocalReference (Name (nameNode x)),[])]
      , functionAttributes = []
      , metadata = []
      }]

    callBin :: Int -> Int -> String -> [Named Instruction]
    callBin x y name =
      [Name (nameNode k) := Call
      { isTailCall = False
      , callingConvention = C
      , returnAttributes = []
      , function = Right (ConstantOperand (C.GlobalReference (Name ("llvm."++name++".f64"))))
      , arguments = [ (LocalReference (Name (nameNode x)),[])
                    , (LocalReference (Name (nameNode y)),[])
                    ]
      , functionAttributes = []
      , metadata = []
      }]
