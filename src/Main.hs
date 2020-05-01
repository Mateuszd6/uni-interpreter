import Control.Monad (when)
import System.Environment (getArgs)
import System.Exit (exitFailure, exitSuccess)

import Error
import Eval
import Parser
import Static
import State

-- Can be set to false to disable static type checking.
doStaticTypeCheck :: Bool
doStaticTypeCheck = True

run :: String -> String -> IO ()
run fname pText = do
  result <- runCtrlT $ do
    parsed <- toCtrlT $ parseProgram pText
    when (doStaticTypeCheck) (toCtrlT $ staticChckProgram parsed)
    evalProgram parsed

  -- Handle any kind of error in one place:
  case ctrlToError result of
    Ok _ -> exitSuccess
    Fail reason -> printErr (getErrorMsg reason fname) >> exitFailure

main :: IO ()
main = do
  args <- getArgs
  case args of
    [] -> getContents >>= run "*stdin*"
    f:_ -> readFile f >>= run f
