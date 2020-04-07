import System.IO ( stdin, hGetContents )
import System.Environment ( getArgs, getProgName )
import System.Exit ( exitFailure, exitSuccess )
import Control.Monad (when)

import qualified AbsLanguage as Abs
import ParLanguage
import LexLanguage
import PrintLanguage
import ErrM

type ParseFun = [Token] -> Err Abs.Program

myLLexer = myLexer

runFile :: ParseFun -> FilePath -> IO ()
runFile p f = putStrLn f >> readFile f >>= run p

tempGetListOfStatements :: Abs.Program -> [Abs.Stmt]
tempGetListOfStatements (Abs.Prog q) = q

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
          putStrLn $ "\nAst has " ++ (show $ length $ tempGetListOfStatements tree) ++ " nodes"
          exitSuccess


showTree :: (Show a, Print a) => a -> IO ()
showTree tree =
  do
    putStrLn $ "\n[Abstract Syntax]\n\n" ++ show tree
    putStrLn $ "\n[Linearized tree]\n\n" ++ printTree tree

main :: IO ()
main = runFile pProgram "./docs/README.txt"
