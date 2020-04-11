module Main where -- TODO: This line is only to kill unused func warnings.

-- TODO: Qualify imports
import Data.Bits (xor)
-- TODO: Explain why strict instead of lazy.
import qualified Data.Map.Strict as Map
-- import System.Environment (getArgs)
import System.Exit (exitFailure, exitSuccess)
import Control.Monad (forM, foldM, foldM_)
import Control.Monad.Trans.Class (lift, MonadTrans(..))

import AbsLanguage as Abs -- TODO: Qualify

import Error
import Parser

showLinCol :: PPos -> String
showLinCol (Just (line, col)) = show line ++ ":" ++ show col -- TODO
showLinCol Nothing = ""

type VarId = Int
type FunId = Int
type TypeId = Int

type Struct = Map.Map String Var

data Var
  = VEmpty
  | VInt Int
  | VBool Bool
  | VString String
  | VStruct { vStructTId :: Int, vStructVars :: Struct }
  deriving (Show)

-- TODO: User can return a struct from a scope which defines it!!!!
-- Probably check in return?

-- TODO: Explain!!
data Scope = Scope
  {
    scopeVars :: Map.Map String VarId,
    scopeFuncs :: Map.Map String FunId,
    scopeTypes :: Map.Map String TypeId
  }
  deriving (Show)

data Store = Store
  {
    storeVars :: Map.Map VarId Var,
    storeFuncs :: Map.Map FunId (Stmt PPos, Scope), -- TODO: make a type instead of using pair?
    -- storeTypes :: Map.Map TypeId () -- TODO!

    nextVarId :: Int,
    nextFuncId :: Int,
    nextTypeId :: Int
  }
  deriving (Show)

data State = State
  {
    counter :: Int,
    stateStore :: Store,
    stateScope :: Scope
  }
  deriving (Show)

-- TODO Kill temps
tempDefaultScope :: Scope
tempDefaultScope = Scope Map.empty Map.empty Map.empty
tempDefaultStore :: Store
tempDefaultStore = Store Map.empty Map.empty 1 1 4 -- TODO: Make sure id don't bind reserved onces.

tempDefaultState :: State
tempDefaultState = State 0 tempDefaultStore tempDefaultScope

dumpState :: State -> IO ()
dumpState s = do
  putStrLn "Dumping state:"
  putStrLn "  Store:"
  putStrLn "    Store vars:"
  mapM_ putStrLn $ map show $ Map.toList $ storeVars $ stateStore s


-- | Create new variable and add it to the state.
addNewVar :: String -> Var -> State -> (VarId, State)
addNewVar name v s@(State _ str@(Store vars _ next _ _) scp@(Scope vnames _ _)) =
  (next, s{
      stateStore = str{ storeVars = Map.insert next v vars,
                        nextVarId = next + 1 },
      stateScope = scp{ scopeVars = Map.insert name next vnames } })

-- typeId is used to determine variable type. We can't use name becasue TODO:
-- explain.
varTypeId :: Var -> Int
varTypeId VEmpty = 0 -- TODO: possibly use Maybe instead?
varTypeId (VInt _) = 1 -- Permitive types have constant typeids.
varTypeId (VBool _) = 2
varTypeId (VString _) = 3
varTypeId (VStruct sId _) = sId -- Structs know their typeids.

ofTypeInt :: Var -> Error Int
ofTypeInt (VInt v) = Ok v
ofTypeInt _ = Fail TypeError

ofTypeBool :: Var -> Error Bool
ofTypeBool (VBool v) = Ok v
ofTypeBool _ = Fail TypeError

ofTypeString :: Var -> Error String
ofTypeString (VString v) = Ok v
ofTypeString _ = Fail TypeError

ofTypeStruct :: TypeId -> Var -> Error Struct
ofTypeStruct desiredId (VStruct tId v)
  | tId == desiredId = Ok v
ofTypeStruct _ _ = Fail TypeError

runFile :: FilePath -> IO ()
runFile f = putStrLn f >> readFile f >>= run

getPos :: Stmt PPos -> Maybe (Int, Int)
getPos (SIf pos _ _) = pos
getPos (SIfElse pos _ _ _) = pos
getPos (SFor pos _ _ _ _) = pos
getPos (SWhile pos _ _) = pos
getPos (SExpr pos _) = pos
getPos (SVDecl pos _) = pos
getPos (SFDecl pos _ _) = pos
getPos (SSDecl pos _ _) = pos
getPos (STDecl pos _ _) = pos
getPos (SAssign pos _  _) = pos
getPos (STAssign pos _ _) = pos
getPos (SIgnore pos _) = pos
getPos (SReturn pos _) = pos
getPos (SBreak pos) = pos
getPos (SCont pos) = pos
getPos (SBlock pos _ _) = pos

-- Evaluates integer binary expresion parametrized by the expression func.
-- TODO: Show iface is needed only for debug, but is fullfiled for our use
evalBinaryExpr :: (Show a, Show r) =>
                  (Var -> Error a) -> -- Convert Var to desired type.
                  (a -> a -> r) -> -- Func performed on wrapped value.
                  (r -> Var) -> -- Ctor that wraps computed value back to Var.
                  PPos ->
                  Expr PPos -> -- LHS expression.
                  Expr PPos -> -- RHS expression.
                  State ->
                  ErrorT IO (Var, State)

-- TODO: Evaluate from right to left like in C
-- TODO: Ppos.
evalBinaryExpr ofType func varCtor _ lhs rhs st = do
  -- The *Conv variables are unwraped value from vars with desired type.

  (evaledL, st') <- evalExpr lhs st
  evaledLConv <- toErrorT $ ofType evaledL
  lift $ print evaledL

  (evaledR, st'') <- evalExpr rhs st'
  evaledRConv <- toErrorT $ ofType evaledR
  lift $ print evaledR

  lift $ putStrLn $ "returning value of: " ++ show (evaledLConv `func` evaledRConv)

  -- Now use the ctor to wrap calculated value back into Var type.
  return (varCtor $ evaledLConv `func` evaledRConv, st'')


evalExpr :: Expr PPos -> State -> ErrorT IO (Var, State)
  -- TODO: Left:
  -- EFnCall a Ident (InvokeExprList a)
  -- EIife a (FunDecl a) (InvokeExprList a)
  -- ELValue a (LValue a)

-- New try:
evalExpr (EInt _ intVal) st = do
  let (res, st') = (VInt $ fromInteger intVal, st)
  lift $ putStrLn $ "Evaluated integer of value: " ++ show intVal
  return (res, st')

evalExpr (EString _ strVal) st = do
  let res = VString strVal
  lift $ putStrLn $ "Evaluated string of value: " ++ show res
  return (res, st)

-- Bfnc doesn't tree booleans as a native types, so unwrap it.
evalExpr (EBool _ boolVal) st = do
  let res = case boolVal of { BTrue _ -> True; BFalse _ -> False }
  lift $ putStrLn $ "Evaluated boolean of value: " ++ show res
  return (VBool res, st)

evalExpr (EPlus p lhs rhs) st = evalBinaryExpr ofTypeInt (+) VInt p lhs rhs st
evalExpr (EMinus p lhs rhs) st = evalBinaryExpr ofTypeInt (-) VInt p lhs rhs st
evalExpr (ETimes p lhs rhs) st = evalBinaryExpr ofTypeInt (*) VInt p lhs rhs st
evalExpr (EDiv p lhs rhs) st = evalBinaryExpr ofTypeInt div VInt p lhs rhs st -- TODO: Double check that
evalExpr (EPow p lhs rhs) st = evalBinaryExpr ofTypeInt (^) VInt p lhs rhs st

evalExpr (EEq p lhs rhs) st = evalBinaryExpr ofTypeInt (==) VBool p lhs rhs st
evalExpr (ENeq p lhs rhs) st = evalBinaryExpr ofTypeInt (/=) VBool p lhs rhs st
evalExpr (EGeq p lhs rhs) st = evalBinaryExpr ofTypeInt (>=) VBool p lhs rhs st
evalExpr (ELeq p lhs rhs) st = evalBinaryExpr ofTypeInt (<=) VBool p lhs rhs st
evalExpr (EGt p lhs rhs) st = evalBinaryExpr ofTypeInt (>) VBool p lhs rhs st
evalExpr (ELt p lhs rhs) st = evalBinaryExpr ofTypeInt (<) VBool p lhs rhs st

evalExpr (ELor p lhs rhs) st = evalBinaryExpr ofTypeBool (||) VBool p lhs rhs st
evalExpr (ELand p lhs rhs) st = evalBinaryExpr ofTypeBool (&&) VBool p lhs rhs st
evalExpr (EXor p lhs rhs) st = evalBinaryExpr ofTypeBool xor VBool p lhs rhs st

evalExpr (ECat p lhs rhs) st = evalBinaryExpr ofTypeString (++) VString p lhs rhs st

-- evalExpr _ _ = toErrorT $ Fail $ NotImplemented "This is madness"
evalExpr _ _ = undefined

evalStmt :: Stmt PPos -> State -> ErrorT IO State
-- evalStmt (SExpr _ _) = undefined -- evalExpr expr undefined >> return ()
evalStmt stmt st = do
  lift $ putStrLn ("tests.txt:" ++ showLinCol (getPos stmt)
                    ++ " evaluating statement in state: "
                    ++ show (counter st))
  return st { counter = counter st + 1 }

-- Evaluate program in initial state.
runProgram :: Program PPos -> ErrorT IO ()
runProgram (Prog _ stmts) = foldM_ (flip evalStmt) tempDefaultState stmts

run :: String -> IO ()
run pText = do
  -- This allows us to handle any kind of error in one place. Whether it's a
  -- parsing error, type error or any kind of an execution error.
  result <- runErrorT (toErrorT (parseProgram pText) >>= runProgram)
  case result of
    Ok () -> exitSuccess
    Fail reason -> putStrLn ("ERROR: " ++ show reason) >> exitFailure

usage :: IO ()
usage = do
  putStrLn $ unlines
    [ "usage: Call with one of the following argument combinations:"
    , "  --help          Display this help message."
    , "  (no arguments)  Parse stdin verbosely."
    , "  (files)         Parse content of files verbosely."
    , "  -s (files)      Silent mode. Parse content of files silently."
    ]
  exitFailure

main :: IO ()
main = do
  let q = tempDefaultState
  print $ q
  let (i, w') = addNewVar "foobar" (VInt 3) q
  putStrLn $ "Added variable with type: " ++ show i
  print $ w'
  putStrLn "\n\n"
  dumpState w'
  putStrLn "\n\n"

  -- Easy part:
  x <- runErrorT $ evalExpr (EInt Nothing 3) tempDefaultState
  print x
  putStrLn "\n\n"

  -- Hard part:
  -- y <- runErrorT $ evalExpr (EPlus Nothing (EBool Nothing (BTrue Nothing)) (EInt Nothing 3)) tempDefaultState
  y <- runErrorT $ evalExpr (EPlus Nothing (EInt Nothing 3) (EInt Nothing 8)) tempDefaultState
  print y
  putStrLn "\n\n"

  -- z <- runErrorT $ evalExpr (ECat Nothing (EString Nothing "Foo") (EInt Nothing 7)) tempDefaultState
  z <- runErrorT $ evalExpr (ECat Nothing (EString Nothing "Foo") (EString Nothing "Bar")) tempDefaultState
  print z
  putStrLn "\n\n"

  w <- runErrorT $ evalExpr (EBool Nothing (BTrue Nothing)) tempDefaultState
  print w
  putStrLn "\n\n\n\n\n"

  -- foobar <- runErrorT $ do
    -- (q, w) <- (\z -> z tempDefaultState) <$> x
    -- return (q, w)
  -- case foobar of
    -- Fail descr -> putStrLn $ "Error occured"
    -- Ok (q, w) -> putStrLn $ show $ (q, w)


  -- y <- runErrorT $ (evalExpr (EInt Nothing 3) >>= (\z -> z tempDefaultState))
  -- print (x <*> (Ok tempDefaultState))

  putStrLn "Here we go again, motherfucker!"
  getContents >>= run
  -- args <- getArgs
  -- case args of
    -- ["--help"] -> usage
    -- [] -> getContents >>= run
    -- "-s":fs -> mapM_ runFile fs
    -- fs -> mapM_ runFile fs
