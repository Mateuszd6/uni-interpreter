-- TODO: Void variable can be the RHS on the deduced type.
-- TODO: It is possible to make a struct of name 'void'

import System.Environment (getArgs)
import System.Exit (exitFailure, exitSuccess)

import Error
import Eval
import Parser
import Static
import State

run :: String -> String -> IO ()
run fname pText = do
  result <- runCtrlT $ do
    parsed <- toCtrlT $ parseProgram pText
    toCtrlT $ staticChckProgram parsed -- Static typecheck
    evalProgram parsed

  -- This allows us to handle any kind of error in one place.
  case ctrlToError result of
    Ok _ -> exitSuccess
    Fail reason -> printErr (getErrorMsg reason fname) >> exitFailure

main :: IO ()
main = do
  args <- getArgs
  case args of
    [] -> getContents >>= run "*stdin*"
    f:_ -> readFile f >>= run f
