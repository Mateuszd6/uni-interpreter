module Main where -- TODO: This line is only to kill unused func warnings.

-- TODO: Void variable can be the RHS on the deduced type.
-- TODO: It is possible to make a struct of name 'void'
-- TODO: Reserved names for structs and functions.

-- TODO: Qualify imports
import Data.Bits (xor)
import qualified Data.Map.Strict as Map -- TODO: Explain why strict instead of lazy.
import System.Environment (getArgs)
import System.Exit (exitFailure, exitSuccess)
import Control.Monad -- TODO: qualify
import Control.Monad.Trans.Class (lift, MonadTrans(..))

import AbsLanguage -- TODO: Qualify

import State
import Parser

showLinCol :: PPos -> String
showLinCol (Just (line, col)) = show line ++ ":" ++ show col ++ ": " -- TODO
showLinCol Nothing = ""

varToInt :: State -> PPos -> Var -> Error Int
varToInt _ _ (VInt v) = Ok v
varToInt _ p (VUninitialized 1) = Fail $ EDVarNotInitialized p
varToInt st p var = Fail $
  EDTypeError "int" (getTypeNameForED (varTypeId var) st) p

varToBool :: State -> PPos -> Var -> Error Bool
varToBool _ _ (VBool v) = Ok v
varToBool _ p (VUninitialized 2) = Fail $ EDVarNotInitialized p
varToBool st p var = Fail $
  EDTypeError "bool" (getTypeNameForED (varTypeId var) st) p

varToString :: State -> PPos -> Var -> Error String
varToString _ _ (VString v) = Ok v
varToString _ p (VUninitialized 3) = Fail $ EDVarNotInitialized p
varToString st p var = Fail $
  EDTypeError "string" (getTypeNameForED (varTypeId var) st) p

-- TODO: why we hardcode the names, and don't use getTypeNameForED?
varToTuple :: State -> PPos -> Var -> Error [Var]
varToTuple _ _ (VTuple v) = Ok v
varToTuple _ p (VUninitialized 4) = Fail $ EDVarNotInitialized p -- TODO: Probably should not happen
varToTuple st p var = Fail $
  EDTypeError "tuple" (getTypeNameForED (varTypeId var) st) p

varToStruct :: State -> PPos -> TypeId -> Var -> Error Struct
varToStruct _ _ desiredId (VStruct tId v)
  | tId == desiredId = Ok v
varToStruct _ p desiredId (VUninitialized tId)
  | tId == desiredId = Fail $ EDVarNotInitialized p
varToStruct st p desiredId var = Fail $
  EDTypeError (getTypeNameForED desiredId st) (getTypeNameForED (varTypeId var) st) p

asStruct :: PPos -> Var -> Error (TypeId, Struct)
asStruct _ (VStruct tId str) = Ok (tId, str)
asStruct p (VUninitialized _) = Fail $ EDVarNotInitialized p
asStruct p _ = Fail $ EDVariableNotStruct p

enforce :: Bool -> ErrorDetail -> Error ()
enforce cond err
  | cond = Ok ()
  | otherwise = Fail err

-- Make sure the var is of the desired type or fail with a TypeError.
enforceType :: Var -> TypeId -> PPos -> State -> Error ()
enforceType v tId p st
  | varTypeId v == tId = Ok ()
  | otherwise = Fail $
    EDTypeError (getTypeNameForED tId st) (getTypeNameForED (varTypeId v) st) p

enforceRetType :: Var -> FRetT -> PPos -> State -> Error ()
enforceRetType (VTuple _) (FRetTSinge _) p _ = Fail $ EDTupleReturned p
enforceRetType v (FRetTSinge tId) p st = enforceType v tId p st
enforceRetType (VTuple vars) (FRetTTuple types) p st = do
  zipped <- errorFromMaybe (EDTupleNumbersDontMatch p (length types) (length vars))
            $ tryZip types vars
  mapM_ (\(t, v) -> enforceType v t p st) zipped

enforceRetType _ (FRetTTuple _) p _ = Fail $ EDValueReturned p

-- Evaluates binary expresion with parametrized the func.
evalBinExpr :: (State -> PPos -> Var -> Error a) -> -- Convert Var to desired type.
               (a -> a -> r) -> -- Func performed on wrapped value.
               (r -> Var) -> -- Ctor that wraps computed value back to Var.
               Expr PPos -> -- LHS expression.
               Expr PPos -> -- RHS expression.
               Maybe (a -> a -> PPos -> Error ()) -> -- Constraints for the vars.
               State ->
               ErrorT IO (Var, State)

-- TODO: Ppos.
evalBinExpr varTo func varCtor lhs rhs constr st = do
  -- The *Conv variables are unwraped value from vars with desired type.

  (evaledL, st') <- evalExpr lhs st
  evaledLConv <- toErrorT $ varTo st (getPos lhs) evaledL

  (evaledR, st'') <- evalExpr rhs st'
  evaledRConv <- toErrorT $ varTo st (getPos rhs) evaledR

  toErrorT $ case constr of
    Just f -> f evaledLConv evaledRConv $ getPos rhs
    Nothing -> Ok ()

  -- Now use the ctor to wrap calculated value back into Var type.
  return (varCtor $ evaledLConv `func` evaledRConv, st'')

evalEqualExpr :: Bool -> Expr PPos -> Expr PPos -> State -> ErrorT IO (Var, State)
evalEqualExpr neg lhs rhs st =
  let tryComp :: Var -> Var -> Error Bool
      tryComp (VInt x) (VInt y) = Ok $ x == y
      tryComp (VBool x) (VBool y) = Ok $ x == y
      tryComp (VString x) (VString y) = Ok $ x == y
      tryComp v1 v2 = Fail $ EDCantCompare (getPos lhs)
                                 (getTypeNameForED (varTypeId v1) st)
                                 (getTypeNameForED (varTypeId v2) st)
  in do
    (evaledL, st') <- evalExpr lhs st
    (evaledR, st'') <- evalExpr rhs st'
    comp <- toErrorT $ tryComp evaledL evaledR
    return (VBool $ (if neg then not else id) comp, st'')

-- Use foldr becasue we wan't to evaluate args from right to left like C does.
-- TODO: https://stackoverflow.com/questions/17055527/lifting-foldr-to-monad
foldrM :: Monad m => (a -> b -> m b) -> b -> [a] -> m b
foldrM _ d [] = return d
foldrM f d (x:xs) = foldrM f d xs >>= f x

appendFst :: [a] -> (a, b) -> ([a], b)
appendFst xs (x, b) = (x:xs, b)

-- Evalulate a list of expressions with foldr, return the list and a new
-- state. Each expression is evaluated in a new state (right to left).
-- TODO: Try to save ppos so that erorss are nicer here.
evalExprsListr :: [Expr PPos] -> State -> ErrorT IO ([Var], State)
evalExprsListr exprs st = do
  (vars, st') <- foldrM (\ex (vars, s) -> appendFst vars <$> evalExpr ex s) ([], st) exprs
  toErrorT $ mapM_ (\(e, v) -> case v of
                                 VTuple _ -> Fail $ EDTupleNotAllowed $ getPos e
                                 _ -> Ok ())
    $ zip exprs vars
  return (vars, st')

fnCallParams :: InvokeExprList PPos -> State -> ErrorT IO ([Var], State)
fnCallParams (IELEmpty _) st = toErrorT $ Ok ([], st)
fnCallParams (IELDefault _ exprs) st = evalExprsListr exprs st

-- Value is returned in a tricky way through 'Flow', so it has to be catched.
evalFunction :: Func -> [Var] -> PPos -> State -> ErrorT IO State
evalFunction func invokeP p st = do
  -- TODO: Fun params are never read-only?
  st' <- toErrorT $
    foldrM (\(par, (pname, tId)) s -> enforceType par tId p s >>
                                      snd <$> createVar pname False par p s)
           st { stateScope = funcScope func } $
           zip invokeP $ funcParams func

  evalStmt (funcBody func) st'

-- TODO: make sure it lays next to assgnStructField.
getStructField :: (TypeId, Struct) -> [String] -> PPos -> State -> Error Var
getStructField (tId, struct) [n] p st =
  errorFromMaybe (EDNoMember p (getTypeNameForED tId st) n) $
    Map.lookup n struct

getStructField (tId, struct) (n:ns) p st = do
  strctDescr <- getTypeDescr tId p st
  destTypeId <- errorFromMaybe (EDNoMember p (getTypeNameForED tId st) n) $
                Map.lookup n $ strctFields strctDescr
  destVar <- errorFromMaybe (EDNoMember p (getTypeNameForED tId st) n) $
             Map.lookup n struct
  destStruct <- snd <$> asStruct p destVar

  getStructField (destTypeId, destStruct) ns p st

getStructField _ [] _ _ = undefined -- TODO: Should not happen.

checkDivByZero :: Int -> Int -> PPos -> Error ()
checkDivByZero _ 0 p = Fail $ EDDivideByZero p
checkDivByZero _ _ _ = Ok ()

evalExpr :: Expr PPos -> State -> ErrorT IO (Var, State)
  -- TODO: Left:
  -- EFnCall a Ident (InvokeExprList a)
  -- EIife a (FunDecl a) (InvokeExprList a)
  -- ELValue a (LValue a)

evalExpr (EInt _ intVal) st = do
  let res = VInt $ fromInteger intVal
  return (res, st)

evalExpr (EString _ strVal) st = do
  -- BNFC seems to keep the string escaped, so we have to unescape it.
  let unescapeImpl :: String -> String -> String
      unescapeImpl acc ('\\':'a':sx) = unescapeImpl ('\a':acc) sx
      unescapeImpl acc ('\\':'b':sx) = unescapeImpl ('\b':acc) sx
      unescapeImpl acc ('\\':'f':sx) = unescapeImpl ('\f':acc) sx
      unescapeImpl acc ('\\':'n':sx) = unescapeImpl ('\n':acc) sx
      unescapeImpl acc ('\\':'r':sx) = unescapeImpl ('\r':acc) sx
      unescapeImpl acc ('\\':'t':sx) = unescapeImpl ('\t':acc) sx
      unescapeImpl acc ('\\':'v':sx) = unescapeImpl ('\v':acc) sx
      unescapeImpl acc ('\\':'\'':sx) = unescapeImpl ('\'':acc) sx
      unescapeImpl acc ('\\':'"':sx) = unescapeImpl ('"':acc) sx
      unescapeImpl acc ('\\':'\\':sx) = unescapeImpl ('\\':acc) sx
      unescapeImpl acc ['\\'] = acc -- \ at the end should not parse.
      unescapeImpl acc (x:sx) = unescapeImpl (x:acc) sx
      unescapeImpl acc [] = acc

      unescape :: String -> String
      unescape = reverse . unescapeImpl [] . tail . init

  return (VString $ unescape strVal, st)

-- Bfnc doesn't treat booleans as a native types, so unwrap it.
evalExpr (EBool _ boolVal) st = do
  let bl = case boolVal of { BTrue _ -> True; BFalse _ -> False }
  return (VBool bl, st)

evalExpr (EPlus _ lhs rhs) st = evalBinExpr varToInt (+) VInt lhs rhs Nothing st
evalExpr (EMinus _ lhs rhs) st = evalBinExpr varToInt (-) VInt lhs rhs Nothing st
evalExpr (ETimes _ lhs rhs) st = evalBinExpr varToInt (*) VInt lhs rhs Nothing st
evalExpr (EDiv _ lhs rhs) st = evalBinExpr varToInt div VInt lhs rhs (Just checkDivByZero) st
evalExpr (EMod _ lhs rhs) st = evalBinExpr varToInt mod VInt lhs rhs (Just checkDivByZero) st
evalExpr (EPow _ lhs rhs) st = evalBinExpr varToInt (^) VInt lhs rhs Nothing st

evalExpr (EGeq _ lhs rhs) st = evalBinExpr varToInt (>=) VBool lhs rhs Nothing st
evalExpr (ELeq _ lhs rhs) st = evalBinExpr varToInt (<=) VBool lhs rhs Nothing st
evalExpr (EGt _ lhs rhs) st = evalBinExpr varToInt (>) VBool lhs rhs Nothing st
evalExpr (ELt _ lhs rhs) st = evalBinExpr varToInt (<) VBool lhs rhs Nothing st

evalExpr (ELor _ lhs rhs) st = evalBinExpr varToBool (||) VBool lhs rhs Nothing st
evalExpr (ELand _ lhs rhs) st = evalBinExpr varToBool (&&) VBool lhs rhs Nothing st
evalExpr (EXor _ lhs rhs) st = evalBinExpr varToBool xor VBool lhs rhs Nothing st

evalExpr (ECat _ lhs rhs) st = evalBinExpr varToString (++) VString lhs rhs Nothing st

-- Operators '==' and '!=' works with any builtin type and have to be
-- treated differently for that reason.
evalExpr (EEq _ lhs rhs) st = evalEqualExpr False lhs rhs st
evalExpr (ENeq _ lhs rhs) st = evalEqualExpr True lhs rhs st

evalExpr (ELValue p (LValueVar _ (Ident vname))) st = do
  (_, v) <- toErrorT $ getVar vname p st
  return (v, st)

evalExpr (ELValue p lv@LValueMemb {}) st = do
  (members, (_, var)) <- toErrorT $ lvalueMem lv st
  lift $ putStrLn $ "Var: " ++ show var
  lift $ putStrLn $ "Members: " ++ show members
  tInfo <- toErrorT $ asStruct p var
  ret <- toErrorT $ getStructField tInfo members p st
  lift $ putStrLn $ "Evaluated lvalue, got: " ++ show ret
  return (ret, st)

evalExpr (ENew p (Ident name)) st
  -- TODO: It is probably not necesarry, because it just won't parse.
  | name == "int" || name == "bool" || name == "string" || name == "void" =
      toErrorT $ Fail $ EDCantBePrimitiveType name p
  | otherwise = do
      -- Create an initailized struct with non-initailized members.
      (tId, strDef) <- toErrorT $ getTypeStruct name p st
      let var = VStruct tId $ Map.map VUninitialized $ strctFields strDef
      return (var, st)

evalExpr (EFnCall p (Ident fname) params) st = do
  lift $ putStrLn $ "Calling function of name: `" ++ fname ++ "'"
  func <- toErrorT $ snd <$> getFunc fname p st
  (invokeParams, st') <- fnCallParams params st
  toErrorT $ enforce (length invokeParams == length (funcParams func))
    $ EDInvalidNumParams p (length $ funcParams func) (length invokeParams)

  let returnsValue = not $ funcReturnsVoid $ funcRetT func
      returnHndlImpl = if returnsValue then expectReturnValue $ funcRetT func
                                       else catchReturnVoid
      returnHndl = returnHndlImpl p . dontAllowBreakContinue

  (ret, st'') <- scope2 (returnHndl . evalFunction func invokeParams p) st'
  return (ret, st'')

-- evalExpr _ _ = toErrorT $ Fail $ NotImplemented "This is madness"

vsIsVarReadOnly :: VarSpec a -> Bool
vsIsVarReadOnly (VSReadOnly _) = True
vsIsVarReadOnly _ = False

-- TODO: Make sure that a variable can't be declared twice in the same scope.
-- evalVarDeclImpl, evalVarAsgnImpl have same signatures. TODO: Not
-- any more? This is important so that we can use them interchangably,
-- e.g. in tuples.
evalVarDeclImpl :: VarSpec PPos -> String -> Var -> PPos -> State -> Error State
evalVarDeclImpl spec vname var p st = snd <$>
  createVar vname (vsIsVarReadOnly spec) var p st

-- TODO: this can be regular Error, not errorT
evalVarAsgnImpl :: String -> Var -> PPos -> State -> Error State
evalVarAsgnImpl vname asgnVal p st = do
  -- (asgnVal, st') <- evalExpr expr st
  -- (vId, var) <- toErrorT $ getVar vname p1 st
  (vId, var) <- getVar vname p st
  enforceType var (varTypeId asgnVal) p st
  setVar vId asgnVal p st

evalVarDecl :: VarDecl PPos -> State -> ErrorT IO State
evalVarDecl (DVDecl p (Ident vname) spec tp) st = do
  tId <- toErrorT $ getTypeId tp st
  toErrorT $ evalVarDeclImpl spec vname (VUninitialized tId) p st

evalVarDecl (DVDeclAsgn p (Ident vname) spec tp expr) st = do
  (v, st') <- evalExpr expr st
  tId <- toErrorT $ getTypeId tp st'
  toErrorT $ enforceType v tId (getPos expr) st'
  toErrorT $ evalVarDeclImpl spec vname v p st'

evalVarDecl (DVDeclDeduce p (Ident vname) spec expr) st = do
    (v, st') <- evalExpr expr st
    toErrorT $ evalVarDeclImpl spec vname v p st'

evalIfStmtImpl :: Expr PPos -> Stmt PPos -> Maybe (Stmt PPos) -> State -> ErrorT IO State
evalIfStmtImpl expr stmt elseStmt st = do
  (v, st') <- evalExpr expr st
  cond <- toErrorT $ varToBool st (getPos expr) v
  lift $ putStrLn $ "evaluated bool: " ++ show cond
  if cond
    then evalStmt stmt st'
    else case elseStmt of
           Just s -> evalStmt s st'
           Nothing -> toErrorT $ Ok st'

evalLoopImpl :: Expr PPos -> Stmt PPos -> Maybe (Stmt PPos) -> State -> ErrorT IO State
evalLoopImpl expr stmt incStmt st = do
  (v, st') <- evalExpr expr st
  cond <- toErrorT $ varToBool st' (getPos expr) v
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

expectReturnValue :: FRetT -> PPos -> ErrorT IO State -> ErrorT IO (Var, State)
expectReturnValue retT p st = do
  err_ <- lift $ runErrorT st
  toErrorT $ case err_ of
    Flow (FRReturn p0 VEmpty) _ -> Fail $ EDReturnVoid p0
    Flow (FRReturn p0 v) st' -> enforceRetType v retT p0 st' >> Ok (v, st')
    Fail r -> Fail r
    _ -> Fail $ EDNoReturn p

dontAllowBreakContinue :: ErrorT IO a -> ErrorT IO a
dontAllowBreakContinue st = do
  err_ <- lift $ runErrorT st
  toErrorT $ case err_ of
    Flow (FRBreak p) _ -> Fail $ EDUnexpectedBreak p
    Flow (FRContinue p) _ -> Fail $ EDUnexpectedContinue p
    _ -> err_

dontAllowReturn :: ErrorT IO a -> ErrorT IO a
dontAllowReturn st = do
  err_ <- lift $ runErrorT st
  toErrorT $ case err_ of
    Flow (FRReturn p _) _ -> Fail $ EDUnexpectedReturn p
    _ -> err_

parseRetType :: FuncRetT PPos -> State -> Error FRetT
parseRetType (FRTSingle _ t) st = FRetTSinge <$> getTypeId t st
parseRetType (FRTTuple _ types) st =
  FRetTTuple <$> foldrM (\a b -> flip (:) b <$> getTypeId a st) [] types

parseRetType (FRTEmpty _) _ = Ok $ FRetTSinge 0

funcReturnsVoid :: FRetT -> Bool
funcReturnsVoid (FRetTSinge 0) = True
funcReturnsVoid _ = False

funcToParams :: FunParams PPos -> State -> Error [Param]
funcToParams (FPEmpty _) _ = Ok []
funcToParams (FPList _ declParams) st =
  mapM (\(DDeclBasic _ (Ident n) t) -> (n,) <$> getTypeId t st) declParams

-- Equivalent to zip if lists have equal lengths, Nothing otherwise
tryZip :: [a] -> [b] -> Maybe [(a, b)]
tryZip [] [] = Just []
tryZip (x:xs) (y:ys) = (:) (x, y) <$> tryZip xs ys
tryZip _ _ = Nothing

tupleAsgnOrDeclImpl :: Bool -> [IdentOrIgnr PPos] -> [Var] -> PPos -> State -> ErrorT IO State
tupleAsgnOrDeclImpl decl targs vs p st = do
  zipped <- toErrorT
            $ errorFromMaybe (EDTupleNumbersDontMatch p (length targs) (length vs))
            $ tryZip targs vs

  let action = if decl then evalVarDeclImpl (VSNone Nothing) else evalVarAsgnImpl -- TODO?
  toErrorT $ foldrM (\(tar, v) s ->
                       case tar of
                         IOIIgnore _ -> Ok s
                         IOIIdent pv (Ident name) -> action name v pv s)
                    st zipped

tupleDeclImpl :: [IdentOrIgnr PPos] -> [Var] -> PPos -> State -> ErrorT IO State
tupleDeclImpl = tupleAsgnOrDeclImpl True

tupleAsgnImpl :: [IdentOrIgnr PPos] -> [Var] -> PPos -> State -> ErrorT IO State
tupleAsgnImpl = tupleAsgnOrDeclImpl False

getStructMemebers :: StrcMembers PPos -> State -> Error [(String, TypeId)]
getStructMemebers (SMEmpty _) _ = Ok []
getStructMemebers (SMDefault _ members) st =
  foldrM (\(DStrMem _ (Ident name) tp) acc -> flip (:) acc . (name,) <$>
                                              getTypeId tp st)
         [] members

-- First member is a list of accessed fields, second is a variable.
lvalueMem :: LValue PPos -> State -> Error ([String], (VarId, Var))
lvalueMem lv st =
  let lvalueMemImpl :: LValue PPos -> [String] -> ([String], PPos, String)
      lvalueMemImpl (LValueVar p0 (Ident name)) fs = (fs, p0, name)
      lvalueMemImpl (LValueMemb _ v (Ident name)) fs = lvalueMemImpl v $ name:fs
      (vmemb, p, vname) = lvalueMemImpl lv []
  in
    (vmemb,) <$> getVar vname p st

assgnStructField :: (TypeId, Struct) -> [String] -> Var -> PPos -> State -> Error Struct
assgnStructField (tId, struct) [n] asgnVal p st = do
  strctDescr <- getTypeDescr tId p st
  destTypeId <- errorFromMaybe (EDNoMember p (getTypeNameForED tId st) n) $
                Map.lookup n $ strctFields strctDescr
  enforceType asgnVal destTypeId p st -- check type of the member
  return $ Map.insert n asgnVal struct

assgnStructField (tId, struct) (n:ns) asgnVal p st = do
  strctDescr <- getTypeDescr tId p st
  destTypeId <- errorFromMaybe (EDNoMember p (getTypeNameForED tId st) n) $
                Map.lookup n $ strctFields strctDescr
  destVar <- errorFromMaybe (EDNoMember p (getTypeNameForED tId st) n) $
             Map.lookup n struct
  destStruct <- snd <$> asStruct p destVar
  modified <- assgnStructField (destTypeId, destStruct) ns asgnVal p st

  return $ Map.insert n (VStruct destTypeId modified) struct

assgnStructField _ [] _ _ _ = undefined -- TODO: Should not happen.

evalStmt :: Stmt PPos -> State -> ErrorT IO State

evalStmt (SBlock _ _ stmts) st = scope (flip (foldM (flip evalStmt)) stmts) st

evalStmt (SAssert p expr) st = do
  (var, st') <- evalExpr expr st
  bool <- toErrorT $ varToBool st' (getPos expr) var
  if not bool
    then toErrorT $ Fail $ EDAssertFail p
    else return st'

evalStmt (SPrint _ exprs) st =
  let printFstRetSnd :: Show a => (a, b) -> IO b
      printFstRetSnd (x, y) = putStr (show x) >> return y
  in
    foldM (\s ex -> evalExpr ex s >>= lift . printFstRetSnd) st exprs


evalStmt (SIf _ expr stmt) st = scope (evalIfStmtImpl expr stmt Nothing) st
evalStmt (SIfElse _ expr stmt elStmt) st = scope (evalIfStmtImpl expr stmt (Just elStmt)) st

-- TODO: Should this be scoped?
evalStmt (SWhile _ expr stmt) st = scope (catchBreak . evalLoopImpl expr stmt Nothing) st

evalStmt e@(SFor _ (Ident iterName) eStart eEnd stmt) st = do
  (vStart, st') <- evalExpr eStart st
  vStartConv <- toErrorT $ varToInt st' (getPos eStart) vStart

  (vEnd, st'') <- evalExpr eEnd st'
  vEndConv <- toErrorT $ varToInt st'' (getPos eEnd) vEnd

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
        s' <- toErrorT $ evalVarDeclImpl (VSNone Nothing) iterName (VInt vStartConv) (getPos e) s
        catchBreak $ evalLoopImpl cmpExpr stmt (Just incStmt) s'

  scope f st''

-- Discard expression result and return new state.
evalStmt (SExpr _ expr) st = snd <$> evalExpr expr st

evalStmt (SVDecl _ vdecl) st = evalVarDecl vdecl st

evalStmt (SFDecl p (Ident fname) (FDDefault _ params bd funRet stmts)) st = do
  lift $ putStrLn $ "Declaring function named `" ++ fname ++ "'."
  -- TODO: Using Sblock is dangerous because bind may eliminate function arguments.
  retT <- toErrorT $ parseRetType funRet st
  fParams <- toErrorT $ funcToParams params st
  let body = SBlock p bd stmts
  toErrorT $ snd <$> createFunc fname body fParams retT p st

evalStmt (SSDecl _ (Ident sname) (SDDefault _ members)) st =
  toErrorT (snd . flip (createStruct sname) st <$> getStructMemebers members st)

evalStmt (STDecl p (TTar _ targs) (EOTTuple _ exprs)) st = do
  (vs, st') <- evalExprsListr exprs st
  tupleDeclImpl targs vs p st'

evalStmt (STAssign p (TTar _ targs) (EOTTuple _ exprs)) st = do
  (vs, st') <- evalExprsListr exprs st
  tupleAsgnImpl targs vs p st'

evalStmt (SIgnore _ (EOTRegular _ expr)) st = snd <$> evalExpr expr st
evalStmt (SIgnore _ (EOTTuple _ exprs)) st = snd <$> evalExprsListr exprs st

evalStmt (STDecl p (TTar _ targs) (EOTRegular _ expr)) st = do
  (var, st') <- evalExpr expr st
  vs <- toErrorT $ varToTuple st' p var
  tupleDeclImpl targs vs p st'

evalStmt (STAssign p (TTar _ targs) (EOTRegular _ expr)) st = do
  (var, st') <- evalExpr expr st
  vs <- toErrorT $ varToTuple st' p var
  tupleAsgnImpl targs vs p st'

-- TODO: Remove once sure SAssign works correctly
-- evalStmt (SAssign p0 (LValueVar p1 (Ident vname)) expr) st = do
  -- (asgnVal, st') <- evalExpr expr st
  -- (vId, var) <- toErrorT $ getVar vname p1 st
  -- toErrorT $ enforceType var (varTypeId asgnVal) p0 st'
  -- TODO: This all should be handled by setvar.

evalStmt (SAssign p lv expr) st = do
  (asgnVal, st') <- evalExpr expr st
  (membs, (vId, var)) <- toErrorT $ lvalueMem lv st'
  toErrorT $ case membs of
    [] -> do -- Assign single variable.
      enforceType var (varTypeId asgnVal) p st'
      setVar vId asgnVal p st'
    _ -> do -- Assign field of a struct.
      (tId, struct) <- asStruct (getPos lv) var
      newStruct <- assgnStructField (tId, struct) membs asgnVal p st'
      setVar vId (VStruct tId newStruct) p st'

evalStmt (SReturn p (RExNone _)) st = toErrorT $ Flow (FRReturn p VEmpty) st
evalStmt (SReturn p (RExRegular _ (EOTRegular _ expr))) st = do
  (result, st') <- evalExpr expr st
  toErrorT $ Flow (FRReturn p result) st'

evalStmt (SReturn p (RExRegular _ (EOTTuple _ exprs))) st = do
  (result, st') <- evalExprsListr exprs st
  toErrorT $ Flow (FRReturn p (VTuple result)) st'

evalStmt (SBreak p) st = toErrorT $ Flow (FRBreak p) st
evalStmt (SCont p) st = toErrorT $ Flow (FRContinue p) st

-- Evaluate program in initial state. TODO: rename to eval program?
runProgram :: Program PPos -> ErrorT IO ()
runProgram (Prog _ stmts) = dontAllowBreakContinue $
                            dontAllowReturn $
                            foldM_ (flip evalStmt) tempDefaultState stmts

run :: String -> String -> IO ()
run fname pText = do
  -- This allows us to handle any kind of error in one place. Whether it's a
  -- parsing error, type error or any kind of an execution error.
  result <- runErrorT (toErrorT (parseProgram pText) >>= runProgram)
  case result of
    Ok () -> exitSuccess
    -- TODO: This can't happen:
    Flow r _ -> printErr ("Flow is broken: " ++ show r ++ "\n") >> exitFailure
    Fail reason -> printErr (errorMsg fname reason) >> exitFailure

-- TODO.
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
main = do -- TODO: Support actual arugments.
  args <- getArgs
  case args of
    [] -> getContents >>= run "*stdin*"
    f:_ -> readFile f >>= run f
