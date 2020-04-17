module Main where -- TODO: This line is only to kill unused func warnings.

-- TODO: Void variable can be the RHS on the deduced type.
-- TODO: It is possible to make a struct of name 'void'
-- TODO: Reserved names for structs and functions.

-- TODO: Qualify imports
import Data.Bits (xor)
-- import System.Environment (getArgs)
import System.Exit (exitFailure, exitSuccess)
import Control.Monad -- TODO: qualify
import Control.Monad.Trans.Class (lift, MonadTrans(..))

import AbsLanguage -- TODO: Qualify

import State
import Parser

showLinCol :: PPos -> String
showLinCol (Just (line, col)) = show line ++ ":" ++ show col ++ ": " -- TODO
showLinCol Nothing = ""

ofTypeInt :: Var -> State -> PPos -> Error Int
ofTypeInt (VInt v) _ _ = Ok v
ofTypeInt (VUninitialized 1) _ p = Fail $ EDVarNotInitialized p
ofTypeInt var st p = Fail $
  EDTypeError "int" (getTypeNameForED (varTypeId var) st) p

ofTypeBool :: Var -> State -> PPos -> Error Bool
ofTypeBool (VBool v) _ _ = Ok v
ofTypeBool (VUninitialized 2) _ p = Fail $ EDVarNotInitialized p
ofTypeBool var st p = Fail $
  EDTypeError "bool" (getTypeNameForED (varTypeId var) st) p

ofTypeString :: Var -> State -> PPos -> Error String
ofTypeString (VString v) _ _ = Ok v
ofTypeString (VUninitialized 3) _ p = Fail $ EDVarNotInitialized p
ofTypeString var st p = Fail $
  EDTypeError "string" (getTypeNameForED (varTypeId var) st) p

ofTypeStruct :: TypeId -> Var -> State -> PPos -> Error Struct
ofTypeStruct desiredId (VStruct tId v) _ _
  | tId == desiredId = Ok v
ofTypeStruct desiredId (VUninitialized tId) _ p
  | tId == desiredId = Fail $ EDVarNotInitialized p
ofTypeStruct desiredId var st p = Fail $
  EDTypeError (getTypeNameForED desiredId st) (getTypeNameForED (varTypeId var) st) p

enforce :: Bool -> ErrorDetail -> Error ()
enforce cond err
  | cond = Ok ()
  | otherwise = Fail err

-- | Make sure the var is of the desired type or fail with a TypeError.
enforceType :: Var -> TypeId -> State -> PPos -> Error ()
enforceType v tId st p
  | varTypeId v == tId = Ok ()
  | otherwise = Fail $
    EDTypeError (getTypeNameForED tId st) (getTypeNameForED (varTypeId v) st) p

runFile :: FilePath -> IO ()
runFile f = putStrLn f >> readFile f >>= run

-- Evaluates integer binary expresion parametrized by the expression func.
-- TODO: Show iface is needed only for debug, but is fullfiled for our use
evalBinExpr :: (Show a, Show r) =>
                  (Var -> State -> PPos -> Error a) -> -- Convert Var to desired type.
                  (a -> a -> r) -> -- Func performed on wrapped value.
                  (r -> Var) -> -- Ctor that wraps computed value back to Var.
                  Expr PPos -> -- LHS expression.
                  Expr PPos -> -- RHS expression.
                  State ->
                  ErrorT IO (Var, State)

-- TODO: Evaluate from right to left like in C
-- TODO: Ppos.
evalBinExpr ofType func varCtor lhs rhs st = do
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

-- Use foldr becasue we wan't to evaluate args from right to left like C does.
-- TODO: https://stackoverflow.com/questions/17055527/lifting-foldr-to-monad
foldrM :: Monad m => (a -> b -> m b) -> b -> [a] -> m b
foldrM _ d [] = return d
foldrM f d (x:xs) = foldrM f d xs >>= f x

fnCallParams :: InvokeExprList PPos -> State -> ErrorT IO ([Var], State)
fnCallParams (IELEmpty _) st = toErrorT $ Ok ([], st)

-- TODO: Try to save ppos so that erorss are nicer here.
fnCallParams (IELDefault _ exprs) st =
  foldrM (\ex (vars, s) -> evalExpr ex s >>= \(v, s') -> return (v:vars, s'))
         ([], st)
         exprs

isBuiltinFunc :: Func -> Bool
isBuiltinFunc (Func fid _ _ _ _)
  | fid >= 1 && fid <= 4 = True -- TODO: No hardcode.
  | otherwise = False

executeBuiltin :: Func -> State -> ErrorT IO State
executeBuiltin (Func 1 _ _ _ _) st = do
  int <- toErrorT $ getVar "val" Nothing st
         >>= (return . snd)
         >>= \v -> ofTypeInt v st Nothing
  lift $ putStr $ show int
  return st

executeBuiltin (Func 2 _ _ _ _) st = do
  bool <- toErrorT $ getVar "val" Nothing st
         >>= (return . snd)
         >>= \v -> ofTypeBool v st Nothing
  lift $ putStr $ show bool
  return st

-- TODO: Would be cool to get line number here.
executeBuiltin (Func 4 _ _ _ _) st = do
  str <- toErrorT $ getVar "val" Nothing st
         >>= (return . snd)
         >>= \v -> ofTypeString v st Nothing
  lift $ putStrLn ("Program execution died: " ++ str)
    >> exitFailure
    >> return undefined -- Obviously not reached.

executeBuiltin _ _ =
  error "This is not a builtin function. This should not happen."

-- Value is returned in a tricky way through 'Flow' type ctor.
evalFunction :: Func -> [Var] -> PPos -> State -> ErrorT IO State
evalFunction func invokeP p st = do
  st' <- toErrorT $
    foldrM (\(par, (pname, tId)) s -> enforceType par tId s p >>
                                      Ok (snd $ createVar pname par s))
           st { stateScope = funcScope func } $
           zip invokeP $ funcParams func

  if isBuiltinFunc func then executeBuiltin func st'
                        else evalStmt (funcBody func) st'

evalExpr :: Expr PPos -> State -> ErrorT IO (Var, State)
  -- TODO: Left:
  -- EFnCall a Ident (InvokeExprList a)
  -- EIife a (FunDecl a) (InvokeExprList a)
  -- ELValue a (LValue a)

evalExpr (EInt _ intVal) st = do
  let res = VInt $ fromInteger intVal
  lift $ putStrLn $ "Evaluated integer of value: " ++ show intVal
  return (res, st)

evalExpr (EString _ strVal) st = do
  -- BNFC seems to keep the enclosing quotes, so remove them.
  -- TODO: Should also unescape double quotes in the middle.
  let res = VString $ tail $ init strVal
  lift $ putStrLn $ "Evaluated string of value: " ++ strVal
  return (res, st)

-- Bfnc doesn't treat booleans as a native types, so unwrap it.
evalExpr (EBool _ boolVal) st = do
  let bl = case boolVal of { BTrue _ -> True; BFalse _ -> False }
  lift $ putStrLn $ "Evaluated boolean of value: " ++ show bl
  return (VBool bl, st)

evalExpr (EPlus _ lhs rhs) st = evalBinExpr ofTypeInt (+) VInt lhs rhs st
evalExpr (EMinus _ lhs rhs) st = evalBinExpr ofTypeInt (-) VInt lhs rhs st
evalExpr (ETimes _ lhs rhs) st = evalBinExpr ofTypeInt (*) VInt lhs rhs st
evalExpr (EDiv _ lhs rhs) st = evalBinExpr ofTypeInt div VInt lhs rhs st -- TODO: Double check that
evalExpr (EPow _ lhs rhs) st = evalBinExpr ofTypeInt (^) VInt lhs rhs st

evalExpr (EEq _ lhs rhs) st = evalBinExpr ofTypeInt (==) VBool lhs rhs st
evalExpr (ENeq _ lhs rhs) st = evalBinExpr ofTypeInt (/=) VBool lhs rhs st
evalExpr (EGeq _ lhs rhs) st = evalBinExpr ofTypeInt (>=) VBool lhs rhs st
evalExpr (ELeq _ lhs rhs) st = evalBinExpr ofTypeInt (<=) VBool lhs rhs st
evalExpr (EGt _ lhs rhs) st = evalBinExpr ofTypeInt (>) VBool lhs rhs st
evalExpr (ELt _ lhs rhs) st = evalBinExpr ofTypeInt (<) VBool lhs rhs st

evalExpr (ELor _ lhs rhs) st = evalBinExpr ofTypeBool (||) VBool lhs rhs st
evalExpr (ELand _ lhs rhs) st = evalBinExpr ofTypeBool (&&) VBool lhs rhs st
evalExpr (EXor _ lhs rhs) st = evalBinExpr ofTypeBool xor VBool lhs rhs st

evalExpr (ECat _ lhs rhs) st = evalBinExpr ofTypeString (++) VString lhs rhs st

evalExpr (ELValue p (LValueVar _ (Ident vname))) st = do
  (_, v) <- toErrorT $ getVar vname p st
  return (v, st)

evalExpr (EFnCall p (Ident fname) params) st = do
  lift $ putStrLn $ "Calling function of name: `" ++ fname ++ "'"
  func <- toErrorT $ getFunc fname p st >>= (return . snd)
  (invokeParams, st') <- fnCallParams params st
  toErrorT $ enforce (length invokeParams == length (funcParams func))
    $ EDInvalidNumParams p (length $ funcParams func) (length invokeParams)

  let returnsValue = funcRetT func /= 0 -- TODO: Hardcode 0 = VEmpty
      returnHndl = if returnsValue then expectReturnValue else catchReturnVoid

  scope2 (returnHndl p . dontAllowBreakContinue . evalFunction func invokeParams p) st'

evalExpr (ELValue _ _) _ = undefined
  -- EDVarNotFound String PPos_

-- evalExpr _ _ = toErrorT $ Fail $ NotImplemented "This is madness"
evalExpr expr _ = do -- TODO: This dies!
  lift $ print expr
  lift $ putStrLn $ "Cant evaluate expr at: " ++ showFCol (getPos expr)
  undefined

-- TODO: Make sure that a variable can't be declared twice in the same scope.
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

evalVarDecl :: VarDecl PPos -> State -> ErrorT IO State
evalVarDecl (DVDecl p (Ident vname) tp) st = do
  tId <- toErrorT $ getTypeId tp st
  evalVarDeclImpl vname tId p (VUninitialized tId) st

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
  cond <- toErrorT $ ofTypeBool v st' (getPos expr)
  lift $ putStrLn $ "evaluated loop condition: " ++ show cond
  if cond
    then do
      st'' <- scope (catchContinue . evalStmt stmt) st'
      st''' <- case incStmt of -- If incStmt is specified evaluate it.
                 Nothing -> return st''
                 Just increm -> evalStmt increm st''
      evalLoopImpl expr stmt incStmt st'''
    else return st'

catchBreak :: ErrorT IO State -> ErrorT IO State
catchBreak st = do
  err_ <- lift $ runErrorT st
  case err_ of
    Flow (FRBreak _) s -> toErrorT $ Ok s
    _ -> toErrorT err_

catchContinue :: ErrorT IO State -> ErrorT IO State
catchContinue st = do
  err_ <- lift $ runErrorT st
  case err_ of
    Flow (FRContinue _) s -> toErrorT $ Ok s
    _ -> toErrorT err_

catchReturnVoid :: PPos -> ErrorT IO State -> ErrorT IO (Var, State)
catchReturnVoid _ st = do
  err_ <- lift $ runErrorT st
  toErrorT $ case err_ of -- TODO: refactor above to work like this one.
    Flow (FRReturn _ VEmpty) st' -> Ok (VEmpty, st')
    Flow (FRReturn p _) _ -> Fail $ EDNoReturnNonVoid p
    Ok st' -> Ok (VEmpty, st')
    Flow x y -> Flow x y
    Fail r -> Fail r

expectReturnValue :: PPos -> ErrorT IO State -> ErrorT IO (Var, State)
expectReturnValue p st = do
  err_ <- lift $ runErrorT st
  toErrorT $ case err_ of -- TODO: refactor above to work like this one.
    Flow (FRReturn p0 VEmpty) _ -> Fail $ EDReturnVoid p0
    Flow (FRReturn _ v) st' -> Ok (v, st')
    Fail r -> Fail r
    _ -> Fail $ EDNoReturn p

dontAllowBreakContinue :: ErrorT IO a -> ErrorT IO a
dontAllowBreakContinue st = do
  err_ <- lift $ runErrorT st
  toErrorT $ case err_ of -- TODO: refactor above to work like this one.
    Flow (FRBreak p) _ -> Fail $ EDUnexpectedBreak p
    Flow (FRContinue p) _ -> Fail $ EDUnexpectedContinue p
    _ -> err_

dontAllowReturn :: ErrorT IO a -> ErrorT IO a
dontAllowReturn st = do
  err_ <- lift $ runErrorT st
  toErrorT $ case err_ of -- TODO: refactor above to work like this one.
    Flow (FRReturn p _) _ -> Fail $ EDUnexpectedReturn p
    _ -> err_

funcRetTToTypeId :: FuncRetT PPos -> State -> Error TypeId
funcRetTToTypeId (FRTSingle _ t) st = getTypeId t st
funcRetTToTypeId (FRTTuple _ _) _ = undefined
funcRetTToTypeId (FRTEmpty _) _ = Ok 0

funcToParams :: FunParams PPos -> State -> Error [Param]
funcToParams (FPEmpty _) _ = Ok []
funcToParams (FPList _ declParams) st =
  mapM (\(DDeclBasic _ (Ident n) t) -> getTypeId t st >>= \tId -> return (n, tId))
       declParams

evalStmt :: Stmt PPos -> State -> ErrorT IO State

evalStmt (SBlock _ _ stmts) st = scope (\s -> foldM (flip evalStmt) s stmts) st

evalStmt (SIf _ expr stmt) st = evalIfStmtImpl expr stmt Nothing st
evalStmt (SIfElse _ expr stmt elStmt) st = evalIfStmtImpl expr stmt (Just elStmt) st

-- TODO: Should this be scoped?
evalStmt (SWhile _ expr stmt) st = scope (catchBreak . evalLoopImpl expr stmt Nothing) st

evalStmt e@(SFor _ (Ident iterName) eStart eEnd stmt) st = do
  (vStart, st') <- evalExpr eStart st
  vStartConv <- toErrorT $ ofTypeInt vStart st' (getPos eStart)

  (vEnd, st'') <- evalExpr eEnd st'
  vEndConv <- toErrorT $ ofTypeInt vEnd st'' (getPos eEnd)

  lift $ putStrLn $ "Loop goes from " ++ show vStartConv ++ " to " ++ show vEndConv

  -- Build artificial statements and use them to control loop flow:
  let (cmpFunc, incFunc) = if vStartConv < vEndConv
                             then (ELeq, EPlus)
                             else (EGeq, EMinus)
      iterLVal = LValueVar Nothing $ Ident iterName
      incRhsExpr = incFunc Nothing (ELValue Nothing iterLVal) $ EInt Nothing 1
      endExpr = EInt Nothing $ toInteger vEndConv
      incStmt = SAssign Nothing iterLVal incRhsExpr
      cmpExpr = cmpFunc Nothing (ELValue Nothing iterLVal) endExpr

  -- Create func taht declares a loop iterator and evaluates loop with given
  -- control statements. Then run the function in a single scope.
  let f s = do
        s' <- evalVarDeclImpl iterName 1 (getPos e) (VInt vStartConv) s
        evalLoopImpl cmpExpr stmt (Just incStmt) s'

  scope f st''

-- Discard expression result and return new state.
evalStmt (SExpr _ expr) st = evalExpr expr st >>= (return . snd)

evalStmt (SVDecl _ vdecl) st = evalVarDecl vdecl st

evalStmt (SFDecl p (Ident fname) (FDDefault _ params bd funRet stmts)) st = do
  lift $ putStrLn $ showFCol p ++ "Declaring function named `" ++ fname ++ "'."
  -- TODO: Using Sblock is dangerous because bind  may eliminate function arguments.
  tId <- toErrorT $ funcRetTToTypeId funRet st
  fParams <- toErrorT $ funcToParams params st
  let body = SBlock p bd stmts

      st' = snd $ createFunc fname body fParams tId st -- TODO: 0 means vempty - don't hardcode
  return st'

evalStmt (STDecl p targ (EOTRegular _ expr)) st = undefined

evalStmt (STDecl p (TTar _ targs) (EOTTuple _ exprs)) st = do
  -- TODO: This appears more in one place, extract it.
  (vs, st') <- foldrM (\ex (vars, s) ->
                          evalExpr ex s >>= \(v, s') -> return ((v, getPos ex):vars, s'))
                      ([], st) exprs
  toErrorT $ enforce (length targs == length exprs)
    $ EDTupleNumbersDontMatch p (length targs) (length exprs)
  lift $ mapM_ (\(v, p0) -> putStrLn $ showFCol p0 ++ show v) vs
  undefined

evalStmt (SAssign p0 (LValueVar p1 (Ident vname)) expr) st = do
  (asgnVal, st') <- evalExpr expr st
  (vId, var) <- toErrorT $ getVar vname p1 st
  toErrorT $ enforceType var (varTypeId asgnVal) st' p0
  toErrorT $ setVar vId asgnVal st' -- TODO: Next this all should be handled by setvar.

evalStmt (SReturn p (RExNone _)) st = toErrorT $ Flow (FRReturn p VEmpty) st
evalStmt (SReturn p (RExRegular _ (EOTRegular _ expr))) st = do
  (result, st') <- evalExpr expr st
  toErrorT $ Flow (FRReturn p result) st'
evalStmt (SReturn _ (RExRegular _ (EOTTuple _ _))) _ = undefined

evalStmt (SBreak p) st = toErrorT $ Flow (FRBreak p) st
evalStmt (SCont p) st = toErrorT $ Flow (FRContinue p) st

evalStmt stmt st = do -- TODO: This dies!
  lift $ putStrLn ("tests.txt:" ++ showLinCol (getPos stmt)
                    ++ " evaluating statement in state: "
                    ++ show (counter st))
  lift $ putStrLn ("tests.txt:" ++ showLinCol (getPos stmt)
                   ++ "No idea how to eavluate " ++ show stmt)

  -- return st { counter = counter st + 1 }
  return undefined

-- Evaluate program in initial state. TODO: rename to eval program?
runProgram :: Program PPos -> ErrorT IO ()
runProgram (Prog _ stmts) = dontAllowBreakContinue $
                            dontAllowReturn $
                            foldM_ (flip evalStmt) tempDefaultState stmts

run :: String -> IO ()
run pText = do
  -- This allows us to handle any kind of error in one place. Whether it's a
  -- parsing error, type error or any kind of an execution error.
  result <- runErrorT (toErrorT (parseProgram pText) >>= runProgram)
  case result of
    Ok () -> exitSuccess
    -- TODO: This can't happen:
    Flow r _ -> putStrLn ("Flow is broken: " ++ show r ++ "\n") >> exitFailure
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
