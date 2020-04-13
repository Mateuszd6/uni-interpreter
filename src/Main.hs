module Main where -- TODO: This line is only to kill unused func warnings.

-- TODO: Qualify imports
import Data.Bits (xor)
-- import System.Environment (getArgs)
import System.Exit (exitFailure, exitSuccess)
import Control.Monad -- TODO: qualify
import Control.Monad.Trans.Class (lift, MonadTrans(..))

import AbsLanguage -- TODO: Qualify

import Error
import State
import Parser

showLinCol :: PPos -> String
showLinCol (Just (line, col)) = show line ++ ":" ++ show col ++ ": " -- TODO
showLinCol Nothing = ""

-- TODO: Refactor the '?': use getTypeNameForED !
ofTypeInt :: Var -> State -> PPos -> Error Int
ofTypeInt (VInt v) _ _ = Ok v
ofTypeInt var st p = Fail $
  EDTypeError "int" (getTypeNameForED (varTypeId var) st) p

ofTypeBool :: Var -> State -> PPos -> Error Bool
ofTypeBool (VBool v) _ _ = Ok v
ofTypeBool var st p = Fail $
  EDTypeError "bool" (getTypeNameForED (varTypeId var) st) p

ofTypeString :: Var -> State -> PPos -> Error String
ofTypeString (VString v) _ _ = Ok v
ofTypeString var st p = Fail $
  EDTypeError "string" (getTypeNameForED (varTypeId var) st) p

ofTypeStruct :: TypeId -> Var -> State -> PPos -> Error Struct
ofTypeStruct desiredId (VStruct tId v) _ _
  | tId == desiredId = Ok v
ofTypeStruct desiredId var st p = Fail $
  EDTypeError (getTypeNameForED desiredId st) (getTypeNameForED (varTypeId var) st) p
runFile :: FilePath -> IO ()
runFile f = putStrLn f >> readFile f >>= run

-- Evaluates integer binary expresion parametrized by the expression func.
-- TODO: Show iface is needed only for debug, but is fullfiled for our use
evalBinaryExpr :: (Show a, Show r) =>
                  (Var -> State -> PPos -> Error a) -> -- Convert Var to desired type.
                  (a -> a -> r) -> -- Func performed on wrapped value.
                  (r -> Var) -> -- Ctor that wraps computed value back to Var.
                  Expr PPos -> -- LHS expression.
                  Expr PPos -> -- RHS expression.
                  State ->
                  ErrorT IO (Var, State)

-- TODO: Evaluate from right to left like in C
-- TODO: Ppos.
evalBinaryExpr ofType func varCtor lhs rhs st = do
  -- The *Conv variables are unwraped value from vars with desired type.

  (evaledL, st') <- evalExpr lhs st
  evaledLConv <- toErrorT $ ofType evaledL st $ getPos lhs
  lift $ print evaledL

  (evaledR, st'') <- evalExpr rhs st'
  evaledRConv <- toErrorT $ ofType evaledR st $ getPos rhs
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
  let res = VInt $ fromInteger intVal
  lift $ putStrLn $ "Evaluated integer of value: " ++ show intVal
  return (res, st)

evalExpr (EString _ strVal) st = do
  let res = VString strVal
  lift $ putStrLn $ "Evaluated string of value: " ++ strVal
  return (res, st)

-- Bfnc doesn't treat booleans as a native types, so unwrap it.
evalExpr (EBool _ boolVal) st = do
  let bl = case boolVal of { BTrue _ -> True; BFalse _ -> False }
  lift $ putStrLn $ "Evaluated boolean of value: " ++ show bl
  return (VBool bl, st)

evalExpr (EPlus _ lhs rhs) st = evalBinaryExpr ofTypeInt (+) VInt lhs rhs st
evalExpr (EMinus _ lhs rhs) st = evalBinaryExpr ofTypeInt (-) VInt lhs rhs st
evalExpr (ETimes _ lhs rhs) st = evalBinaryExpr ofTypeInt (*) VInt lhs rhs st
evalExpr (EDiv _ lhs rhs) st = evalBinaryExpr ofTypeInt div VInt lhs rhs st -- TODO: Double check that
evalExpr (EPow _ lhs rhs) st = evalBinaryExpr ofTypeInt (^) VInt lhs rhs st

evalExpr (EEq _ lhs rhs) st = evalBinaryExpr ofTypeInt (==) VBool lhs rhs st
evalExpr (ENeq _ lhs rhs) st = evalBinaryExpr ofTypeInt (/=) VBool lhs rhs st
evalExpr (EGeq _ lhs rhs) st = evalBinaryExpr ofTypeInt (>=) VBool lhs rhs st
evalExpr (ELeq _ lhs rhs) st = evalBinaryExpr ofTypeInt (<=) VBool lhs rhs st
evalExpr (EGt _ lhs rhs) st = evalBinaryExpr ofTypeInt (>) VBool lhs rhs st
evalExpr (ELt _ lhs rhs) st = evalBinaryExpr ofTypeInt (<) VBool lhs rhs st

evalExpr (ELor _ lhs rhs) st = evalBinaryExpr ofTypeBool (||) VBool lhs rhs st
evalExpr (ELand _ lhs rhs) st = evalBinaryExpr ofTypeBool (&&) VBool lhs rhs st
evalExpr (EXor _ lhs rhs) st = evalBinaryExpr ofTypeBool xor VBool lhs rhs st

evalExpr (ECat _ lhs rhs) st = evalBinaryExpr ofTypeString (++) VString lhs rhs st

evalExpr (ELValue p (LValueVar _ (Ident vname))) st = do
  (_, v) <- toErrorT $ getVar vname p st
  return (v, st)

evalExpr (ELValue _ _) _ = undefined

  -- EDVarNotFound String PPos_

-- evalExpr _ _ = toErrorT $ Fail $ NotImplemented "This is madness"
evalExpr expr _ = do -- TODO: This dies!
  lift $ print expr
  lift $ print $ "Cant evaluate expr at: " ++ showFCol (getPos expr)
  undefined

-- TODO: Make sure that a variable can't be declared twice in the same scope.
-- TODO: Handle the situation when the name already exists in the scope.  I
--       think it's fine, becasue the name is replaced in the scope when
--       inserting.
-- TODO: Don't use vempty refactor to use maybe!
-- TODO: This should probably fail when the variable already exists. Double
--       check that scenario to make sure state is not changed!!
evalVarDeclImpl :: String -> TypeId -> PPos -> Var -> State -> ErrorT IO State
evalVarDeclImpl vname tId p var st = do
  lift $ putStrLn ("tests.txt:" ++ showLinCol p
                   ++ "Declaring variable: " ++ vname
                   ++ " of type: " ++ show tId
                   ++ " with value: " ++ show var)
  let (vid, st') = createVar vname var st
  lift $ putStrLn $ "created variable of varId: " ++ show vid
  lift $ dumpState st'
  return st'

-- | Make sure the var is of the desired type or fail with a TypeError.
enforceType :: Var -> TypeId -> State -> PPos -> Error ()
enforceType v tId st p
  | varTypeId v == tId = Ok ()
  | otherwise = Fail $ EDTypeError (getTypeNameForED tId st) (getTypeNameForED (varTypeId v) st) p

evalVarDecl :: VarDecl PPos -> State -> ErrorT IO State
evalVarDecl (DVDecl p (Ident vname) tp) st = do
  tId <- toErrorT $ getTypeId tp st
  evalVarDeclImpl vname tId p VEmpty st

evalVarDecl (DVDeclAsgn p (Ident vname) tp expr) st = do
  (v, st') <- evalExpr expr st
  tId <- toErrorT $ getTypeId tp st'

  -- Enforce that given type is same as the type of the RHS expresion.
  toErrorT $ enforceType v tId st $ getPos expr
  evalVarDeclImpl vname tId p v st'

evalVarDecl (DVDeclDeduce p (Ident vname) expr) st = do
    (v, st') <- evalExpr expr st
    let tId = varTypeId v -- New type is equal to the rhs type.
    evalVarDeclImpl vname tId p v st'

evalIfStmtImpl :: Expr PPos -> Stmt PPos -> Maybe (Stmt PPos) -> State -> ErrorT IO State
evalIfStmtImpl expr stmt elseStmt st = do
  (v, st') <- evalExpr expr st
  cond <- toErrorT $ ofTypeBool v st (getPos expr)
  lift $ putStrLn $ "evaluated bool: " ++ show cond
  if cond
    then evalStmt stmt st'
    else case elseStmt of
           Just s -> evalStmt s st'
           Nothing -> toErrorT $ Ok st'

evalLoopImpl :: Expr PPos -> Stmt PPos -> Maybe (Stmt PPos) -> State -> ErrorT IO State
evalLoopImpl expr stmt incStmt st = do
  (v, st') <- evalExpr expr st
  cond <- toErrorT $ ofTypeBool v st (getPos expr)
  lift $ putStrLn $ "evaluated loop condition: " ++ show cond
  if cond
    then do
      st'' <- case incStmt of -- If incStmt is specified evaluate it.
                Nothing -> return st'
                Just increm -> evalStmt increm st'
      evalLoopImpl expr stmt incStmt st''
    else return st'

evalStmt :: Stmt PPos -> State -> ErrorT IO State

evalStmt (SIf _ expr stmt) st = evalIfStmtImpl expr stmt Nothing st
evalStmt (SIfElse _ expr stmt elStmt) st = evalIfStmtImpl expr stmt (Just elStmt) st

evalStmt (SWhile _ expr stmt) st = evalLoopImpl expr stmt Nothing st

evalStmt e@(SFor _ (Ident iterName) eStart eEnd stmt) st = do
  (vStart, st') <- evalExpr eStart st
  vStartConv <- toErrorT $ ofTypeInt vStart st' (getPos eStart)

  (vEnd, st'') <- evalExpr eEnd st'
  vEndConv <- toErrorT $ ofTypeInt vEnd st'' (getPos eEnd)

  lift $ putStrLn $ "Loop goes from " ++ show vStartConv ++ " to " ++ show vEndConv

  -- Build artificial statements and use them to control loop flow:
  let incFunc = if vStartConv < vEndConv then EPlus else EMinus
      iterLValExpr = LValueVar Nothing $ Ident iterName
      incRhsExpr = incFunc Nothing (ELValue Nothing iterLValExpr) $ EInt Nothing 1
      endExpr = EInt Nothing $ toInteger vEndConv
      incStmt = SAssign Nothing iterLValExpr incRhsExpr
      cmpExpr = ENeq Nothing (ELValue Nothing iterLValExpr) endExpr

  -- Declare the loop iterator and evaluate loop with control statements.
  st''' <- evalVarDeclImpl iterName 1 (getPos e) (VInt vStartConv) st'' -- TODO: 1 is int tid. Don't hardcode!
  evalLoopImpl cmpExpr stmt (Just incStmt) st'''
  -- TODO: Iterator visible after leaving the loop?

-- Discard expression result and return new state.
evalStmt (SExpr _ expr) st = evalExpr expr st >>= (return . snd)

evalStmt (SVDecl _ vdecl) st = evalVarDecl vdecl st

evalStmt (SAssign p0 (LValueVar p1 (Ident vname)) expr) st = do
  (vId, var) <- toErrorT $ getVar vname p1 st
  (asgnVal, st') <- evalExpr expr st
  toErrorT $ enforceType var (varTypeId asgnVal) st' p0
  toErrorT $ setVar vId st' -- TODO: Next this all should be handled by setvar.

-- evalStmt (SExpr _ _) = undefined -- evalExpr expr undefined >> return ()
evalStmt stmt st = do -- TODO: This dies!
  lift $ putStrLn ("tests.txt:" ++ showLinCol (getPos stmt)
                    ++ " evaluating statement in state: "
                    ++ show (counter st))
  lift $ putStrLn ("tests.txt:" ++ showLinCol (getPos stmt)
                   ++ "No idea how to eavluate " ++ show stmt)

  -- return st { counter = counter st + 1 }
  return undefined

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
    Fail reason -> print reason >> exitFailure

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
  {-
  let q = tempDefaultState
  print q
  let (_, q') = createVar "foobar" (VInt 3) q
  let (_, q'') = createVar "baz" (VString "tutututut") q'
  let (_, q''') = createVar "is_pure" (VBool True) q''
  dumpState q'''

  let vv = getVar "is_pure_" (Just (1, 2)) q'''
  print vv

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

  putStrLn "Here we go again, motherfucker!" -}
  getContents >>= run
  -- args <- getArgs
  -- case args of
    -- ["--help"] -> usage
    -- [] -> getContents >>= run
    -- "-s":fs -> mapM_ runFile fs
    -- fs -> mapM_ runFile fs
