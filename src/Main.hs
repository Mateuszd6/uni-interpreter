{-# LANGUAGE TupleSections #-}

-- TODO: Void variable can be the RHS on the deduced type.
-- TODO: It is possible to make a struct of name 'void'
-- TODO: Scan docs.
-- TODO: Changes docs.
-- TODO: Fix the issue with *unknown* type.
-- TODO: Read only params in 'bad' tests.

import Control.Monad (foldM, foldM_, when)
import Control.Monad.Trans.Class (lift, MonadTrans(..))
import Data.Bits (xor)
import Data.Foldable (foldl')
import System.IO (hFlush, stdout)
import System.IO.Error (catchIOError)
import System.Environment (getArgs)
import System.Exit (exitFailure, exitSuccess)
import Text.Read (readMaybe)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

import AbsLanguage
import Common
import Parser
import State

asInt :: State -> PPos -> Var -> Error Int
asInt _ _ (VInt v) = Ok v
asInt _ p (VUninitialized 1) = Fail $ EDVarNotInitialized p
asInt st p var = Fail $ EDTypeError "int" (getTypeName (varTypeId var) st) p

asBool :: State -> PPos -> Var -> Error Bool
asBool _ _ (VBool v) = Ok v
asBool _ p (VUninitialized 2) = Fail $ EDVarNotInitialized p
asBool st p var = Fail $ EDTypeError "bool" (getTypeName (varTypeId var) st) p

asString :: State -> PPos -> Var -> Error String
asString _ _ (VString v) = Ok v
asString _ p (VUninitialized 3) = Fail $ EDVarNotInitialized p
asString st p var = Fail $ EDTypeError "string" (getTypeName (varTypeId var) st) p

-- TODO: why we hardcode the names, and don't use getTypeName?
asTuple :: State -> PPos -> Var -> Error [Var]
asTuple _ _ (VTuple v) = Ok v
asTuple _ p (VUninitialized 4) = Fail $ EDVarNotInitialized p -- TODO: Probably should not happen
asTuple st p var = Fail $ EDTypeError "tuple" (getTypeName (varTypeId var) st) p

asStruct :: State -> PPos -> Var -> Error (TypeId, Struct)
asStruct _ _ (VStruct tId str) = Ok (tId, str)
asStruct _ p (VUninitialized _) = Fail $ EDVarNotInitialized p
asStruct _ p _ = Fail $ EDVariableNotStruct p

-- Abstract commonly occurinng patter of evaluating var and then checking its type.
exprAs :: (State -> PPos -> Var -> Error a) -> Expr PPos -> State -> ErrorT IO (a, State)
exprAs asF expr st = do
  (v, st') <- evalExpr expr st
  x <- toErrorT $ asF st (getPos expr) v
  return (x, st')

enforceParamLengthEqual :: PPos -> Int -> Int -> Error ()
enforceParamLengthEqual p expected got =
  when (expected /= got) $ Fail $ EDInvalidNumParams p expected got

-- Make sure the var is of the desired type or fail with a TypeError.
enforceType :: Var -> TypeId -> PPos -> State -> Error ()
enforceType v tId p st = when (varTypeId v /= tId) $ Fail $
                         EDTypeError (getTypeName tId st)
                                     (getTypeName (varTypeId v) st) p

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
                                 (getTypeName (varTypeId v1) st)
                                 (getTypeName (varTypeId v2) st)
  in do
    (evaledL, st') <- evalExpr lhs st
    (evaledR, st'') <- evalExpr rhs st'
    comp <- toErrorT $ tryComp evaledL evaledR
    return (VBool $ (if neg then not else id) comp, st'')

-- Use foldr becasue we evaluate args from right to left like C does.
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
                                 VTuple _ -> Fail $ EDTupleNotAllowed (getPos e)
                                 _ -> Ok ())
    $ zip exprs vars
  return (vars, st')

fnCallParams :: InvokeExprList PPos -> State -> ErrorT IO ([Var], State)
fnCallParams (IELEmpty _) st = return ([], st)
fnCallParams (IELDefault _ exprs) st = evalExprsListr exprs st

-- Value is returned in a tricky way through 'Flow', so it has to be catched.
evalFunction :: Func -> [Var] -> PPos -> State -> ErrorT IO State
evalFunction func invokeP p st = do
  -- TODO: Fun params are never read-only?
  let foo = case funcBind func of
        Just x -> (scopeCnt st + 1, x) : bindVars st
        Nothing -> bindVars st

  st' <- toErrorT $
    foldrM (\(par, (pname, spec, tId)) s -> enforceType par tId p s >>
                                            snd <$> createVar pname (varSpecReadOnly spec) par p s)
           st { scopeCnt = scopeCnt st + 1,
                stateScope = funcScope func,
                bindVars = foo } $
           zip invokeP (funcParams func)

  evalStmt (funcBody func) st'

getStructFieldType :: TypeId -> String -> PPos -> State -> Error TypeId
getStructFieldType tId field p st = do
  strctDescr <- getTypeDescr tId p st
  errorFromMaybe (EDNoMember p (getTypeName tId st) field) $
                 Map.lookup field (strctFields strctDescr)

getStructFieldImpl :: (TypeId, Struct) -> String -> PPos -> State
                   -> Error (TypeId, Struct)
getStructFieldImpl (tId, struct) field p st = do
  destTypeId <- getStructFieldType tId field p st
  destVar <- errorFromMaybe (EDNoMember p (getTypeName tId st) field) $
             Map.lookup field struct
  (destTypeId, ) . snd <$> asStruct st p destVar

getStructField :: (TypeId, Struct) -> [String] -> PPos -> State -> Error Var
getStructField (tId, struct) [n] p st =
  errorFromMaybe (EDNoMember p (getTypeName tId st) n) $
    Map.lookup n struct

getStructField (tId, struct) (n:ns) p st = do
  (destTypeId, destStruct) <- getStructFieldImpl (tId, struct) n p st
  getStructField (destTypeId, destStruct) ns p st

getStructField _ [] _ _ = undefined -- Would not parse.

assgnStructField :: (TypeId, Struct) -> [String] -> Var -> PPos -> State -> Error Struct
assgnStructField (tId, struct) [n] asgnVal p st = do
  destTypeId <- getStructFieldType tId n p st
  enforceType asgnVal destTypeId p st -- check type of the member
  return $ Map.insert n asgnVal struct

assgnStructField (tId, struct) (n:ns) asgnVal p st = do
  (destTypeId, destStruct) <- getStructFieldImpl (tId, struct) n p st
  modified <- assgnStructField (destTypeId, destStruct) ns asgnVal p st
  return $ Map.insert n (VStruct destTypeId modified) struct

assgnStructField _ [] _ _ _ = undefined -- Would not parse.

getBindVars :: Bind PPos -> State -> Error (Maybe (Set.Set VarId))
getBindVars (BdPure _) _ = Ok $ Just $ Set.fromList []
getBindVars (BdPureAlt _) _ = Ok $ Just $ Set.fromList[]
getBindVars (BdNone _) _ = Ok Nothing
getBindVars (BdDefault p idents) st = fromMaybeList <$>
  foldM (\acc name -> (\x -> (:) x <$> acc) . fst <$> getVar name p st)
        (Just [])
        (map (\(Ident str) -> str) idents)
  where
    fromMaybeList :: Ord a => Maybe [a] -> Maybe (Set.Set a)
    fromMaybeList (Just l) = Just $ Set.fromList l
    fromMaybeList Nothing = Nothing

enforce0div :: Int -> Int -> PPos -> Error ()
enforce0div _ 0 p = Fail $ EDDivideByZero p
enforce0div _ _ _ = Ok ()

enfoceBindedVarsAreInBlock :: String -> Func -> PPos -> State -> Error ()
enfoceBindedVarsAreInBlock fname func p st = do
  bind <- errorFromMaybe (EDCantUseWiderBind fname p) $ funcBind func
  if setContainsAll (snd $ head $ bindVars st) bind
    then Ok ()
    else Fail $ EDCantUseWiderBind fname p
  where
    setContainsAll :: Set.Set VarId -> Set.Set VarId -> Bool
    setContainsAll wider = foldr (\vId acc -> acc && Set.member vId wider) True

enforceFnCallBindRules :: String -> Func -> PPos -> State -> Error ()
enforceFnCallBindRules fname func p st =
  let definedAt = fScopeN func
      lastBindAt = fst $ head $ bindVars st
  in
    if definedAt >= lastBindAt
      then Ok ()
      else enfoceBindedVarsAreInBlock fname func p st

returnHanlder :: Func -> PPos -> (ErrorT IO State -> ErrorT IO (Var, State))
returnHanlder func p =
  returnHndl p . dontAllowBreakContinue
  where
    returnHndl = if not $ funcReturnsVoid $ funcRetT func
                   then expectReturnValue (funcRetT func)
                   else catchReturnVoid

evalExpr :: Expr PPos -> State -> ErrorT IO (Var, State)

evalExpr (EInt _ intVal) st = do
  let res = VInt $ fromInteger intVal
  return (res, st)

evalExpr (EString _ strVal) st = return (VString $ unescape strVal, st)

-- Bfnc doesn't treat booleans as a native types, so unwrap it.
evalExpr (EBool _ boolVal) st = do
  let bl = case boolVal of { BTrue _ -> True; BFalse _ -> False }
  return (VBool bl, st)

evalExpr (EPlus _ lhs rhs) st = evalBinExpr asInt (+) VInt lhs rhs Nothing st
evalExpr (EMinus _ lhs rhs) st = evalBinExpr asInt (-) VInt lhs rhs Nothing st
evalExpr (ETimes _ lhs rhs) st = evalBinExpr asInt (*) VInt lhs rhs Nothing st
evalExpr (EDiv _ lhs rhs) st = evalBinExpr asInt div VInt lhs rhs (Just enforce0div) st
evalExpr (EMod _ lhs rhs) st = evalBinExpr asInt mod VInt lhs rhs (Just enforce0div) st
evalExpr (EPow _ lhs rhs) st = evalBinExpr asInt (^) VInt lhs rhs Nothing st
evalExpr (EGeq _ lhs rhs) st = evalBinExpr asInt (>=) VBool lhs rhs Nothing st
evalExpr (ELeq _ lhs rhs) st = evalBinExpr asInt (<=) VBool lhs rhs Nothing st
evalExpr (EGt _ lhs rhs) st = evalBinExpr asInt (>) VBool lhs rhs Nothing st
evalExpr (ELt _ lhs rhs) st = evalBinExpr asInt (<) VBool lhs rhs Nothing st
evalExpr (ELor _ lhs rhs) st = evalBinExpr asBool (||) VBool lhs rhs Nothing st
evalExpr (ELand _ lhs rhs) st = evalBinExpr asBool (&&) VBool lhs rhs Nothing st
evalExpr (EXor _ lhs rhs) st = evalBinExpr asBool xor VBool lhs rhs Nothing st
evalExpr (ECat _ lhs rhs) st = evalBinExpr asString (++) VString lhs rhs Nothing st

-- Operators '==' and '!=' works with any builtin type.
evalExpr (EEq _ lhs rhs) st = evalEqualExpr False lhs rhs st
evalExpr (ENeq _ lhs rhs) st = evalEqualExpr True lhs rhs st

evalExpr (ELValue p (LValueVar _ (Ident vname))) st = toErrorT $
  (, st) . snd <$> getVar vname p st

evalExpr (ELValue p lv@LValueMemb {}) st = do
  (members, (_, var)) <- toErrorT $ lvalueMem lv st
  structVar <- toErrorT $ asStruct st p var
  toErrorT $ (, st) <$> getStructField structVar members p st

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
  func <- toErrorT $ snd <$> getFunc fname p st
  (invokeParams, st') <- fnCallParams params st

  toErrorT $ enforceParamLengthEqual p (length $ funcParams func) (length invokeParams)
  toErrorT $ enforceFnCallBindRules fname func p st

  scope2 (returnHanlder func p . evalFunction func invokeParams p) Nothing st'

evalExpr (EIife p (FDDefault _ params bind funRet stmts) invkParams) st = do
  retT <- toErrorT $ parseRetType funRet st
  fParams <- toErrorT $ funcToParams params st
  (invokeParams, st') <- fnCallParams invkParams st
  bindV <- toErrorT $ getBindVars bind st'

  let body = SBlock p (BdNone Nothing) stmts
      func = Func (-1) body retT fParams bindV (stateScope st') (scopeCnt st')

  toErrorT $ enforceParamLengthEqual p (length $ funcParams func) (length invokeParams)
  -- No need to enforce bind rules, as iife is defined in the scope its used.

  scope2 (returnHanlder func p . evalFunction func invokeParams p) Nothing st'

evalExpr (EScan _ types) st = do
  toErrorT $ mapM_ enforceIsBultinType types
  line <- lift $ catchIOError getLine (\_ -> return [])
  let failedParse t l = (False, (False, toUninitialized t) : l)
      vars = reverse $ snd $
             foldl' (\(parsedSoFar, l) (t, s) ->
                        if parsedSoFar
                        then maybe (failedParse t l)
                             ((True, ) . flip (:) l <$> (True, ))
                             (s >>= parse t)
                        else failedParse t l)
                    (True, [])
                    (zipMaybe types (words line))
      parsedSuccessfully = countIf fst vars

  return (VTuple $ VInt parsedSuccessfully : map snd vars, st)
  where
    parse :: Type PPos -> String -> Maybe Var
    parse (TInt _) s = VInt <$> (readMaybe :: String -> Maybe Int) s
    parse (TBool _) s = VBool <$> (readMaybe :: String -> Maybe Bool) s
    parse (TString _) s = Just $ VString s
    parse _ _ = undefined -- Safe, because we've checked for these types before

    toUninitialized :: Type PPos -> Var
    toUninitialized t = VUninitialized $ nofail $ getTypeId t st

varSpecReadOnly :: VarSpec a -> Bool
varSpecReadOnly (VSReadOnly _) = True
varSpecReadOnly _ = False

evalVarDeclImpl :: VarSpec PPos -> String -> Var -> PPos -> State -> Error State
evalVarDeclImpl spec vname var p st = snd <$>
  createVar vname (varSpecReadOnly spec) var p st

evalVarAsgnImpl :: String -> Var -> PPos -> State -> Error State
evalVarAsgnImpl vname asgnVal p st = do
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
  cond <- toErrorT $ asBool st (getPos expr) v
  lift $ putStrLn $ "evaluated bool: " ++ show cond
  if cond
    then evalStmt stmt st'
    else case elseStmt of
           Just s -> evalStmt s st'
           Nothing -> return st'

evalLoopImpl :: Expr PPos -> Stmt PPos -> Maybe (Stmt PPos) -> State -> ErrorT IO State
evalLoopImpl expr stmt incStmt st = do
  (v, st') <- evalExpr expr st
  cond <- toErrorT $ asBool st' (getPos expr) v
  lift $ putStrLn $ "evaluated loop condition: " ++ show cond
  if cond
    then do
      st'' <- scope (catchContinue . evalStmt stmt) Nothing st'
      st''' <- maybe (return st'') (`evalStmt` st'') incStmt -- Eval inc stmt if specified
      evalLoopImpl expr stmt incStmt st'''
    else return st'

catchBreak :: ErrorT IO State -> ErrorT IO State
catchBreak st = do
  err_ <- lift $ runErrorT st
  case err_ of
    Flow (FRBreak _) s -> return s
    _ -> toErrorT err_

catchContinue :: ErrorT IO State -> ErrorT IO State
catchContinue st = do
  err_ <- lift $ runErrorT st
  case err_ of
    Flow (FRContinue _) s -> return s
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
parseRetType (FRTEmpty _) _ = Ok $ FRetTSinge 0
parseRetType (FRTSingle _ t) st = FRetTSinge <$> getTypeId t st
parseRetType (FRTTuple _ types) st = FRetTTuple <$>
  foldrM (\a b -> flip (:) b <$> getTypeId a st) [] types

funcReturnsVoid :: FRetT -> Bool
funcReturnsVoid (FRetTSinge 0) = True
funcReturnsVoid _ = False

funcToParams :: FunParams PPos -> State -> Error [Param]
funcToParams (FPEmpty _) _ = Ok []
funcToParams (FPList _ declParams) st =
  mapM (\(DDeclBasic _ (Ident n) spec t) -> (n, spec, ) <$> getTypeId t st) declParams

tupleAsgnOrDeclImpl :: Bool -> [IdentOrIgnr PPos] -> [Var] -> PPos -> State
                    -> ErrorT IO State
tupleAsgnOrDeclImpl decl targs vs p st = do
  zipped <- toErrorT
            $ errorFromMaybe (EDTupleNumbersDontMatch p (length targs) (length vs))
            $ tryZip targs vs

  let action = if decl then evalVarDeclImpl (VSNone Nothing) else evalVarAsgnImpl
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
  foldrM (\(DStrMem _ (Ident name) tp) acc -> flip (:) acc . (name, ) <$>
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
    (vmemb, ) <$> getVar vname p st

evalStmt :: Stmt PPos -> State -> ErrorT IO State

evalStmt (SBlock _ bind stmts) st = do
  bindV <- toErrorT $ getBindVars bind st
  scope (flip (foldM (flip evalStmt)) stmts) bindV st

evalStmt (SAssert p expr) st = do
  (var, st') <- evalExpr expr st
  bool <- toErrorT $ asBool st' (getPos expr) var
  if not bool
    then toErrorT $ Fail $ EDAssertFail p
    else return st'

evalStmt (SPrint _ exprs) st = do
  st' <- foldM (\s ex -> evalExpr ex s >>= lift . printFstRetSnd) st exprs
  lift $ hFlush stdout -- Just in case
  return st'

evalStmt (SIf _ expr stmt) st = scope (evalIfStmtImpl expr stmt Nothing) Nothing st
evalStmt (SIfElse _ expr stmt elStmt) st = scope (evalIfStmtImpl expr stmt (Just elStmt)) Nothing st

evalStmt (SWhile _ expr stmt) st = scope (catchBreak . evalWhileImpl expr stmt) Nothing st
  where
    evalWhileImpl :: Expr PPos -> Stmt PPos -> State -> ErrorT IO State
    evalWhileImpl expr stmt st = do
      (cond, st') <- exprAs asBool expr st
      if cond
        then do
          st'' <- scope (catchContinue . evalStmt stmt) Nothing st'
          evalWhileImpl expr stmt st''
        else return st'

evalStmt e@(SFor p (Ident iterName) eStart eEnd stmt) st = do
  (start, st') <- exprAs asInt eStart st
  (end, st'') <- exprAs asInt eEnd st'
  let iters = if start <= end then [start .. end] else reverse [end .. start]
      loopStep i s = do
        s' <- toErrorT $ evalVarDeclImpl (VSReadOnly p) iterName (VInt i) (getPos e) s
        catchContinue $ evalStmt stmt s'
      loopBody s = catchBreak $ foldM (\acc i -> scope (loopStep i) Nothing acc) s iters

  scope loopBody Nothing st''

-- Discard expression result and return new state.
evalStmt (SExpr _ expr) st = snd <$> evalExpr expr st

evalStmt (SVDecl _ vdecl) st = evalVarDecl vdecl st

evalStmt (SFDecl p (Ident fname) (FDDefault _ params bd funRet stmts)) st = do
  lift $ putStrLn $ "Declaring function named `" ++ fname ++ "'."
  retT <- toErrorT $ parseRetType funRet st
  fParams <- toErrorT $ funcToParams params st
  bindV <- toErrorT $ getBindVars bd st
  let body = SBlock p (BdNone Nothing) stmts
  toErrorT $ snd <$> createFunc fname body fParams retT bindV p st

evalStmt (SSDecl p (Ident sname) (SDDefault _ members)) st = do
  strMembs <- toErrorT $ getStructMemebers members st
  toErrorT $ snd <$> createStruct sname p strMembs st

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
  vs <- toErrorT $ asTuple st' p var
  tupleDeclImpl targs vs p st'

evalStmt (STAssign p (TTar _ targs) (EOTRegular _ expr)) st = do
  (var, st') <- evalExpr expr st
  vs <- toErrorT $ asTuple st' p var
  tupleAsgnImpl targs vs p st'

evalStmt (SAssign p lv expr) st = do
  (asgnVal, st') <- evalExpr expr st
  (membs, (vId, var)) <- toErrorT $ lvalueMem lv st'

  toErrorT $ case membs of
    [] -> do -- Assign single variable.
      enforceType var (varTypeId asgnVal) p st'
      setVar vId asgnVal p st'
    _ -> do -- Assign field of a struct.
      (tId, struct) <- asStruct st' (getPos lv) var
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

evalProgram :: Program PPos -> ErrorT IO ()
evalProgram (Prog _ stmts) = dontAllowBreakContinue $ dontAllowReturn $
                             foldM_ (flip evalStmt) initialState stmts

run :: String -> String -> IO ()
run fname pText = do
  -- This allows us to handle any kind of error in one place.
  result <- runErrorT (toErrorT (parseProgram pText) >>= evalProgram)
  case result of
    Ok () -> exitSuccess
    -- TODO: This can't happen:
    Flow r _ -> printErr ("Flow is broken: " ++ show r ++ "\n") >> exitFailure
    Fail reason -> printErr (errorMsg fname reason) >> exitFailure


main :: IO ()
main = do
  args <- getArgs
  case args of
    [] -> getContents >>= run "*stdin*"
    f:_ -> readFile f >>= run f
