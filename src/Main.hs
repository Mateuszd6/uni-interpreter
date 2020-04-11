-- automatically generated by BNF Converter
module Main where

import qualified Data.Map as Map

-- TODO: Qualify imports
import System.IO ()
-- import System.Environment (getArgs)
import System.Exit (exitFailure, exitSuccess)
import Control.Monad (foldM, foldM_)
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

-- TODO: Explain!!
data ScopeNames = ScopeNames
  {
    scopeVars :: Map.Map String VarId,
    scopeFuncs :: Map.Map String FunId,
    scopeTypes :: Map.Map String TypeId
  }
  deriving (Show)

data Var
  = VEmpty
  | VInt Int
  | VBool Bool
  | VString String
  | VStruct { vStructTId :: Int, vStructVars :: Map.Map String Var }
  deriving (Show)

-- TODO: User can return a struct from a scope which defines it!!!!
-- Probably check in return?

data State = State
  {
    counter :: Int,
    stateNames :: ScopeNames,
    stateVars :: Map.Map VarId Var,
    stateFuncs :: Map.Map FunId (Stmt PPos, ScopeNames) -- TODO: make a type instead of using pair?
    -- stateTypes :: Map.Map TypeId () -- TODO!
  }
  deriving (Show)

-- TODO Kill temps
tempDefaultScopeNames :: ScopeNames
tempDefaultScopeNames = ScopeNames Map.empty Map.empty Map.empty
tempDefaultState :: State
tempDefaultState = State 0 tempDefaultScopeNames Map.empty Map.empty

-- typeId is used to determine variable type. We can't use name becasue TODO:
-- explain.
varGetTypeId :: Var -> Int
varGetTypeId VEmpty = 0 -- TODO: possibly use Maybe instead?
varGetTypeId (VInt _) = 1 -- Permitive types have constant typeids.
varGetTypeId (VBool _) = 2
varGetTypeId (VString _) = 3
varGetTypeId (VStruct sId _) = sId -- Structs know their typeids.

-- TODO: produce more or refactor.
ofTypeInt :: Var -> Error Int
ofTypeInt (VInt v) = Ok_ v
ofTypeInt _ = Fail_ TypeError

-- TODO: These don't have to be ErrorT IO's, but rather regular Errors

-- TODO: Refactor or kill
ofTypeInt_ :: (Var, State) -> ErrorT IO (Int, State)
ofTypeInt_ (VInt v, s) = ErrorT $ return $ Ok_ (v, s)
ofTypeInt_ (_, _) = ErrorT $ return $ Fail_ TypeError -- TODO: Include state for dump info?

-- TODO: produce more or refactor.
ofTypeString :: Var -> Error String
ofTypeString (VString v) = Ok_ v
ofTypeString _ = Fail_ TypeError

ofTypeString_ :: (Var, State) -> ErrorT IO (String, State)
ofTypeString_ (VString str, s) = ErrorT $ return $ Ok_ (str, s)
ofTypeString_ (_, _) = ErrorT $ return $ Fail_ TypeError -- TODO: Include state
                                                         -- for dump info?

-- parseProg :: [Token] -> Error (Program PPos)
-- parseProg = Par.pProgram
-- lexProg :: String -> [Token]
-- lexProg = Par.myLexer

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

toListOfStmts :: Program a -> [Stmt a]
toListOfStmts (Prog _ statements) = statements

-- TODO: Pass pasring pos.

-- Evaluates integer binary expresion parametrized by the expression func.
evalIntExpr :: (Int -> Int -> Int) ->
               PPos -> Expr PPos -> Expr PPos -> State ->
               ErrorT IO (Var, State)

evalIntExpr func _ lhs rhs st = do
  (evaledL, st') <- evalExpr lhs st >>= ofTypeInt_
  lift $ print evaledL

  (evaledR, st'') <- evalExpr rhs st' >>= ofTypeInt_
  lift $ print evaledR

  lift $ putStrLn $ "returning value of: " ++ show (evaledL + evaledR)
  return (VInt $ evaledL `func` evaledR, st'')

evalExpr :: Expr PPos -> State -> ErrorT IO (Var, State)

-- New try:
evalExpr (EInt _ intVal) st = do
  let (res, st') = (VInt $ fromInteger intVal, st)
  lift $ putStrLn $ "Evaluated integer of value: " ++ show intVal
  return (res, st')

evalExpr (EString _ strVal) st = do
  let res = VString strVal
  lift $ putStrLn $ "Evaluated integer of value: " ++ show res
  return (res, st)

-- Bfnc doesn't tree booleans as a native types, so unwrap it.
evalExpr (EBool _ boolVal) st = do
  let res = case boolVal of { BTrue _ -> True; BFalse _ -> False }
  lift $ putStrLn $ "Evaluated integer of value: " ++ show res
  return (VBool res, st)

evalExpr (EPlus p lhs rhs) st = evalIntExpr (+) p lhs rhs st
evalExpr (EMinus p lhs rhs) st = evalIntExpr (-) p lhs rhs st
evalExpr (ETimes p lhs rhs) st = evalIntExpr (*) p lhs rhs st
evalExpr (EDiv p lhs rhs) st = evalIntExpr div p lhs rhs st -- TODO: Double check that
evalExpr (EPow p lhs rhs) st = evalIntExpr (^) p lhs rhs st

evalExpr (ECat _ lhs rhs) st = do
  (evaledL, st') <- evalExpr lhs st >>= ofTypeString_
  lift $ print evaledL

  (evaledR, st'') <- evalExpr rhs st' >>= ofTypeString_
  lift $ print evaledR

  lift $ putStrLn $ "returning value of: " ++ show (evaledL ++ evaledR)
  return (VString $ evaledL ++ evaledR, st'')

-- evalExpr _ _ = ErrorT $
  -- return $ Fail_ $ NotImplemented "This is madness"
evalExpr _ _ = undefined

-- Old:

-- evalExpr expr s = ErrorT $
  -- return $ Fail_ $ SuperBadErrorThatBasicallyShouldNotHappen "This is madness"

-- evalExpr expr s = do
  -- lift $ putStrLn $ ("tests.txt:" ++ (showLinCol Nothing)
                     -- ++ " evaluating expression:\n  " ++ (show expr))
  -- return $ (VEmpty, s)


-- evalExpr (EInt _ intVal) st = do
  -- let (res, st') = (VInt $ fromInteger intVal, st)
  -- putStrLn $ "Evaluated integer of value: " ++ show intVal
  -- return $ (Ok_ res, st')

-- evalExpr (EPlus _ lhs rhs) st = do
  -- (evaledL, st') <- evalExpr lhs st
  -- (evaledR, st'') <- evalExpr rhs st'
  -- let l1 = ofTypeInt evaledL
  -- let l2 = ofTypeInt evaledR

  -- putStrLn $ ("Should add two expressions:\n  "
              -- ++ show evaledL ++ "\n  "
              -- ++ show evaledR ++ "\n  "
              -- ++ "which gives: " ++ (show $ addError l1 l2))
  -- return (addError l1 l2, st'')

-- evalExpr expr s = do
  -- putStrLn $ "tests.txt:" ++ (showLinCol $ Nothing)
              -- ++ " evaluating expression:\n  " ++ show expr
  -- return (Ok_ VEmpty, s)

-- evalExpr _ _ = undefined

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
  result <- runErrorT $ ((ErrorT $ return $ parseProgram pText) >>= runProgram)
  case result of
    Ok_ () -> exitSuccess
    Fail_ reason -> putStrLn ("ERROR: " ++ show reason) >> exitFailure

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
  -- Easy part:
  x <- runErrorT $ evalExpr (EInt Nothing 3) tempDefaultState
  print x
  putStrLn "\n\n"

  -- Hard part:
  y <- runErrorT $ evalExpr (EPlus Nothing (EBool Nothing (BTrue Nothing)) (EInt Nothing 3)) tempDefaultState
  -- y <- runErrorT $ evalExpr (EPlus Nothing (EInt Nothing 3) (EInt Nothing 8)) tempDefaultState
  print y
  putStrLn "\n\n"

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
    -- Fail_ descr -> putStrLn $ "Error occured"
    -- Ok_ (q, w) -> putStrLn $ show $ (q, w)


  -- y <- runErrorT $ (evalExpr (EInt Nothing 3) >>= (\z -> z tempDefaultState))
  -- print (x <*> (Ok_ tempDefaultState))

  putStrLn "Hello I hate haskell"
  getContents >>= run
  -- args <- getArgs
  -- case args of
    -- ["--help"] -> usage
    -- [] -> getContents >>= run
    -- "-s":fs -> mapM_ runFile fs
    -- fs -> mapM_ runFile fs
