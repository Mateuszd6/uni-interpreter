-- TODO: Check imports.
import System.IO ( stdin, hGetContents )
import System.Environment ( getArgs, getProgName )
import System.Exit ( exitFailure, exitSuccess )
import Control.Monad (when)
import Control.Applicative (Applicative(..), Alternative(..))
import Control.Monad (liftM, ap)

import qualified Data.HashMap.Strict as HMap

import qualified AbsLanguage as Abs
import ParLanguage
import LexLanguage
import PrintLanguage
import ErrM

type ParseFun = [Token] -> Err Abs.Program

-- Aliases for commonly use maps:
type VarMap = HMap.HashMap String Var

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
    vars :: VarMap
  }
  deriving (Show)

getVar :: VarMap -> String -> Maybe Var
getVar vars name = HMap.lookup name vars

myLLexer = myLexer

runFile :: ParseFun -> FilePath -> IO ()
runFile p f = putStrLn f >> readFile f >>= run p

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

run :: ParseFun -> String -> IO ()
run p s =
  let
    ts = myLLexer s
  in
    case p ts of
      Bad s ->
        do
          putStrLn "\nParse Failed..."
          putStrLn "\nTokens:\n"
          putStrLn $ show ts
          putStrLn s
          exitFailure
      Ok tree ->
        do
          putStrLn "\nParse Successful!"
          showTree tree
          putStrLn $ test $ tempGetListOfStatements tree
          exitSuccess


showTree :: (Show a, Print a) => a -> IO ()
showTree tree =
  do
    putStrLn $ "\n[Abstract Syntax]\n\n" ++ show tree
    putStrLn $ "\n[Linearized tree]\n\n" ++ printTree tree

main :: IO ()
main = runFile pProgram "./docs/README.txt"
