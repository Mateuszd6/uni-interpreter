-- TODO: Check imports.
import System.Exit (exitFailure, exitSuccess)
import Control.Monad (liftM, ap)

import qualified Data.Map.Strict as Map

import qualified AbsLanguage as Abs
import qualified ParLanguage as Par (myLexer, pProgram)
import LexLanguage (Token)
import PrintLanguage
import ErrM

parseProg :: [Token] -> Err Abs.Program
parseProg = Par.pProgram

lexProg :: String -> [Token]
lexProg = Par.myLexer

-- Aliases for commonly use maps:
type VarMap = Map.Map String Var

-- A struct is a map from field names to variables
newtype Struct = Struct VarMap
  deriving (Show)

data Var
  = VInt Int
  | Vbool Bool
  | VString String
  | VStruct Struct
  deriving (Show)

newtype ProgramState = ProgramState
  {
    psvars :: VarMap
  }
  deriving (Show)

getVar :: VarMap -> String -> Maybe Var
getVar vars name = Map.lookup name vars

runFile :: FilePath -> IO ()
runFile f = putStrLn f >> readFile f >>= run

tempGetListOfStatements :: Abs.Program -> [Abs.Stmt]
tempGetListOfStatements (Abs.Prog q) = q

test :: [Abs.Stmt] -> String
test = foldr (\x acc -> (case x of
                           Abs.SIf _ _ -> "IF statement\n"
                           _ -> "No idea\n") ++ acc) ""

-- MonadicMadness
newtype State s a = State { runState :: (s -> (a, s)) }

instance Monad (State s)
  where
    return a = State (\s -> (a,s))
    (State x) >>= f = State (\s -> let (v, s') = x s in runState (f v) s')

instance Functor (State s) where
  fmap = liftM

instance Applicative (State s) where
  pure  = return
  (<*>) = ap

run :: String -> IO ()
run s =
  case parseProg $ lexProg s of
    Bad errMsg -> do
      putStrLn "\nParse Failed..."
      putStrLn "\nTokens:\n"
      putStrLn $ show $ lexProg s -- TODO: Don't repeat lex program!
      putStrLn errMsg
      exitFailure
    Ok tree -> do
      putStrLn "\nParse Successful!"
      showTree tree
      putStrLn $ test $ tempGetListOfStatements tree
      exitSuccess

showTree :: (Show a, Print a) => a -> IO ()
showTree tree = do
  putStrLn $ "\n[Abstract Syntax]\n\n" ++ show tree
  putStrLn $ "\n[Linearized tree]\n\n" ++ printTree tree

testmap :: VarMap
testmap = foldl (\acc (x, y) -> Map.insert x y acc) Map.empty testvals

testvals :: [(String, Var)]
testvals = [("Var_1", VInt 1), ("Var_2", VInt 1), ("Var_3", VString "TestString")]

main :: IO ()
main = do
  -- runFile "./docs/README.txt"
  -- putStrLn $ show (testmap :: VarMap)
  putStrLn $ show $ getVar testmap "Var_1"
