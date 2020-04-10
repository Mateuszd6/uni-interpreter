-- automatically generated by BNF Converter
module Main where

import qualified Data.Map as Map

import System.IO ()
import System.Environment (getArgs)
import System.Exit (exitFailure, exitSuccess)
import Control.Monad (forM_)

import qualified ParLanguage as Par
import AbsLanguage as Abs -- TODO: Qualify
-- import PrintLanguage

import ErrM

-- Do lexing, then parsing. Lexing can't fail and by combing these functions we
-- don't have to include LexLanguage in this file.
parseProgram :: String -> Err (Program ParsingPos)
parseProgram = Par.pProgram . Par.myLexer

type ParsingPos = Maybe (Int, Int)

showLinCol :: ParsingPos -> String
showLinCol (Just (line, col)) = (show line) ++ ":" ++ (show col) -- TODO
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
  | VStruct { vStructTId :: Int, vStructVars :: (Map.Map String Var) }
  deriving (Show)

-- TODO: User can return a struct from a scope which defines it!!!!
-- Probably check in return?

data State = State
  {
    stateNames :: ScopeNames,
    stateVars :: Map.Map VarId Var,
    stateFuncs :: Map.Map FunId (Stmt ParsingPos, ScopeNames) -- TODO: make a type instead of using pair?
    -- stateTypes :: Map.Map TypeId () -- TODO!
  }
  deriving (Show)

-- typeId is used to determine variable type. We can't use name becasue TODO:
-- explain.
varGetTypeId :: Var -> Int
varGetTypeId VEmpty = 0 -- TODO: possibly use Maybe instead?
varGetTypeId (VInt _) = 1 -- Permitive types have constant typeids.
varGetTypeId (VBool _) = 2
varGetTypeId (VString _) = 3
varGetTypeId (VStruct sId _) = sId -- Structs know their typeids.

-- TODO: produce more or refactor.
ofTypeInt :: Var -> Err Int
ofTypeInt (VInt v) = Ok v
ofTypeInt _ = Error TypeError

-- parseProg :: [Token] -> Err (Program ParsingPos)
-- parseProg = Par.pProgram
-- lexProg :: String -> [Token]
-- lexProg = Par.myLexer

runFile :: FilePath -> IO ()
runFile f = putStrLn f >> readFile f >>= run

getPos :: Stmt ParsingPos -> Maybe (Int, Int)
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

addErr :: Err Int -> Err Int -> Err Int
addErr (Ok a) (Ok b) = Ok $ a + b
addErr _ _ = Bad "Should not happen"

evalExpr :: Expr ParsingPos -> State -> IO (Var, State)

evalExpr (EInt _ intVal) st = do
  let (res, st') = (VInt $ fromInteger intVal, st)
  putStrLn $ "Evaluated integer of value: " ++ show intVal
  return (res, st')

evalExpr (EPlus _ lhs rhs) st = do
  (evaledL, st') <- evalExpr lhs st
  (evaledR, st'') <- evalExpr rhs st'
  let l1 = ofTypeInt evaledL
  let l2 = ofTypeInt evaledR

  putStrLn $ ("Should add two expressions:\n  "
              ++ show evaledL ++ "\n  "
              ++ show evaledR ++ "\n  "
              ++ "which gives: " ++ (show $ addErr l1 l2))
  return (VInt 99, st'')

evalExpr expr s = do
  putStrLn $ "tests.txt:" ++ (showLinCol $ Nothing)
              ++ " evaluating expression:\n  " ++ show expr
  return (VEmpty, s)


evalStmt :: Stmt ParsingPos -> State -> IO () -- TODO: IO State
evalStmt (SExpr _ (expr)) _ = evalExpr expr undefined >> return ()
evalStmt stmt _ = putStrLn $ ("tests.txt:" ++ (showLinCol $ getPos stmt)
                              ++ " evaluating statement.")

run :: String -> IO ()
run s = case parseProgram s of
          Bad errMsg -> do
            putStrLn "\nParse              Failed...\n"
            putStrLn "Tokens:"
            -- putStrLn $ show ts
            putStrLn $ "Message: " ++ errMsg
            exitFailure
          Ok tree -> do
            putStrLn "Parse Successful!"
            -- putStrLn $ printTree tree
            -- putStrLn $ "\npos: " ++ (show tree)
            putStrLn $ "\nNum statements: " ++ (show $ length $ toListOfStmts tree)
            -- putStr $ foldr (++) "" $ map (astDumpStmt dumpStateInitial) (toListOfStmts tree)
            forM_ (toListOfStmts tree) (\x -> evalStmt x undefined)
            exitSuccess

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
  args <- getArgs
  case args of
    ["--help"] -> usage
    [] -> getContents >>= run
    "-s":fs -> mapM_ runFile fs
    fs -> mapM_ runFile fs
