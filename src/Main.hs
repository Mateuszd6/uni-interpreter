{-# LANGUAGE TupleSections #-}

-- TODO: Void variable can be the RHS on the deduced type.
-- TODO: It is possible to make a struct of name 'void'
-- TODO: Scan docs.
-- TODO: Changes docs.
-- TODO: Fix the issue with *unknown* type.

import Control.Monad (foldM, foldM_)
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

-- TODO:
-- varToStruct :: State -> PPos -> TypeId -> Var -> Error Struct
-- varToStruct _ _ desiredId (VStruct tId v)
  -- | tId == desiredId = Ok v
-- varToStruct _ p desiredId (VUninitialized tId)
  -- | tId == desiredId = Fail $ EDVarNotInitialized p
-- varToStruct st p desiredId var = Fail $
  -- EDTypeError (getTypeNameForED desiredId st) (getTypeNameForED (varTypeId var) st) p

asStruct :: PPos -> Var -> Error (TypeId, Struct)
asStruct _ (VStruct tId str) = Ok (tId, str)
asStruct p (VUninitialized _) = Fail $ EDVarNotInitialized p
asStruct p _ = Fail $ EDVariableNotStruct p

enforceParamLengthEqual :: PPos -> Int -> Int -> Error ()
enforceParamLengthEqual p expected got
  | expected == got = Ok ()
  | otherwise = Fail $ EDInvalidNumParams p expected got

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
  let foo = case funcBind func of
        Just x -> (scopeCnt st + 1, x) : bindVars st
        Nothing -> bindVars st

  st' <- toErrorT $
    foldrM (\(par, (pname, tId)) s -> enforceType par tId p s >>
                                      snd <$> createVar pname False par p s)
           st { scopeCnt = scopeCnt st + 1,
                stateScope = funcScope func,
                bindVars = foo } $
           zip invokeP (funcParams func)

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

enforceNoDivByZero :: Int -> Int -> PPos -> Error ()
enforceNoDivByZero _ 0 p = Fail $ EDDivideByZero p
enforceNoDivByZero _ _ _ = Ok ()

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

evalExpr :: Expr PPos -> State -> ErrorT IO (Var, State)

evalExpr (EInt _ intVal) st = do
  let res = VInt $ fromInteger intVal
  return (res, st)

evalExpr (EString _ strVal) st = return (VString $ unescape strVal, st)

-- Bfnc doesn't treat booleans as a native types, so unwrap it.
evalExpr (EBool _ boolVal) st = do
  let bl = case boolVal of { BTrue _ -> True; BFalse _ -> False }
  return (VBool bl, st)

evalExpr (EPlus _ lhs rhs) st = evalBinExpr varToInt (+) VInt lhs rhs Nothing st
evalExpr (EMinus _ lhs rhs) st = evalBinExpr varToInt (-) VInt lhs rhs Nothing st
evalExpr (ETimes _ lhs rhs) st = evalBinExpr varToInt (*) VInt lhs rhs Nothing st
evalExpr (EDiv _ lhs rhs) st = evalBinExpr varToInt div VInt lhs rhs (Just enforceNoDivByZero) st
evalExpr (EMod _ lhs rhs) st = evalBinExpr varToInt mod VInt lhs rhs (Just enforceNoDivByZero) st
evalExpr (EPow _ lhs rhs) st = evalBinExpr varToInt (^) VInt lhs rhs Nothing st
evalExpr (EGeq _ lhs rhs) st = evalBinExpr varToInt (>=) VBool lhs rhs Nothing st
evalExpr (ELeq _ lhs rhs) st = evalBinExpr varToInt (<=) VBool lhs rhs Nothing st
evalExpr (EGt _ lhs rhs) st = evalBinExpr varToInt (>) VBool lhs rhs Nothing st
evalExpr (ELt _ lhs rhs) st = evalBinExpr varToInt (<) VBool lhs rhs Nothing st
evalExpr (ELor _ lhs rhs) st = evalBinExpr varToBool (||) VBool lhs rhs Nothing st
evalExpr (ELand _ lhs rhs) st = evalBinExpr varToBool (&&) VBool lhs rhs Nothing st
evalExpr (EXor _ lhs rhs) st = evalBinExpr varToBool xor VBool lhs rhs Nothing st
evalExpr (ECat _ lhs rhs) st = evalBinExpr varToString (++) VString lhs rhs Nothing st

-- Operators '==' and '!=' works with any builtin type.
evalExpr (EEq _ lhs rhs) st = evalEqualExpr False lhs rhs st
evalExpr (ENeq _ lhs rhs) st = evalEqualExpr True lhs rhs st

evalExpr (ELValue p (LValueVar _ (Ident vname))) st = toErrorT $
  (, st) <$> snd <$> getVar vname p st

evalExpr (ELValue p lv@LValueMemb {}) st = do
  (members, (_, var)) <- toErrorT $ lvalueMem lv st
  structVar <- toErrorT $ asStruct p var
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
  lift $ putStrLn $ "Calling function of name: `" ++ fname ++ "'"
  func <- toErrorT $ snd <$> getFunc fname p st
  (invokeParams, st') <- fnCallParams params st

  toErrorT $ enforceParamLengthEqual p (length $ funcParams func) (length invokeParams)
  toErrorT $ enforceFnCallBindRules fname func p st

  -- TODO: __ Copied somewhere else  __
  let returnsValue = not $ funcReturnsVoid $ funcRetT func
      returnHndlImpl = if returnsValue then expectReturnValue $ funcRetT func
                                       else catchReturnVoid
      returnHndl = returnHndlImpl p . dontAllowBreakContinue

  scope2 (returnHndl . evalFunction func invokeParams p) Nothing st'

evalExpr (EIife p (FDDefault _ params bind funRet stmts) invkParams) st = do
  -- TODO: A lot of this is copypasted.
  retT <- toErrorT $ parseRetType funRet st
  fParams <- toErrorT $ funcToParams params st
  (invokeParams, st') <- fnCallParams invkParams st

  let body = SBlock p (BdNone Nothing) stmts
      func = Func (-1) body retT fParams Nothing (stateScope st') (scopeCnt st')
      returnsValue = not $ funcReturnsVoid $ funcRetT func
      returnHndlImpl = if returnsValue then expectReturnValue $ funcRetT func
                                       else catchReturnVoid
      returnHndl = returnHndlImpl p . dontAllowBreakContinue

  toErrorT $ enforceParamLengthEqual p (length $ funcParams func) (length invokeParams)

  bindV <- toErrorT $ getBindVars bind st'
  scope2 (returnHndl . evalFunction func invokeParams p) bindV st'

evalVarDeclImpl :: VarSpec PPos -> String -> Var -> PPos -> State -> Error State
evalVarDeclImpl spec vname var p st = snd <$>
  createVar vname (varSpecReadOnly spec) var p st
  where
    varSpecReadOnly :: VarSpec a -> Bool
    varSpecReadOnly (VSReadOnly _) = True
    varSpecReadOnly _ = False

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
      st'' <- scope (catchContinue . evalStmt stmt) Nothing st'
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
  mapM (\(DDeclBasic _ (Ident n) t) -> (n, ) <$> getTypeId t st) declParams

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

evalStmt (SBlock _ bind stmts) st = do
  bindV <- toErrorT $ getBindVars bind st
  scope (flip (foldM (flip evalStmt)) stmts) bindV st

evalStmt (SAssert p expr) st = do
  (var, st') <- evalExpr expr st
  bool <- toErrorT $ varToBool st' (getPos expr) var
  if not bool
    then toErrorT $ Fail $ EDAssertFail p
    else return st'

evalStmt (SPrint _ exprs) st = do
  st' <- foldM (\s ex -> evalExpr ex s >>= lift . printFstRetSnd) st exprs
  lift $ hFlush stdout -- Just in case
  return st'

evalStmt (SScan p types) st = do
  toErrorT $ mapM_ enforceIsBultinType types
  line <- lift $ catchIOError getLine (\_ -> return [])
  let failedParse t l = (False, (False, toUninitialized t) : l)
      vars = reverse $ snd $
             foldl' (\(parsedSoFar, l) (t, s) ->
                        if parsedSoFar
                          then maybe (failedParse t l)
                               ((True, ) . flip (:) l <$> (True, )) (s >>= parse t)
                          else failedParse t l)
                    (True, [])
                    (zipMaybe types (words line))
      parsedSuccessfully = countIf fst vars

  toErrorT $ Flow (FRReturn p (VTuple $ VInt parsedSuccessfully : map snd vars)) st
  where
    parse :: Type PPos -> String -> Maybe Var
    parse (TInt _) s = VInt <$> (readMaybe :: String -> Maybe Int) s
    parse (TBool _) s = VBool <$> (readMaybe :: String -> Maybe Bool) s
    parse (TString _) s = Just $ VString s
    parse _ _ = undefined -- Safe, because we've checked for these types before

    toUninitialized :: Type PPos -> Var
    toUninitialized t = VUninitialized $ nofail $ getTypeId t st

evalStmt (SIf _ expr stmt) st = scope (evalIfStmtImpl expr stmt Nothing) Nothing st
evalStmt (SIfElse _ expr stmt elStmt) st = scope (evalIfStmtImpl expr stmt (Just elStmt)) Nothing st

-- TODO: Should this be scoped?
evalStmt (SWhile _ expr stmt) st = scope (catchBreak . evalLoopImpl expr stmt Nothing) Nothing st

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

  scope f Nothing st''

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
  vs <- toErrorT $ varToTuple st' p var
  tupleDeclImpl targs vs p st'

evalStmt (STAssign p (TTar _ targs) (EOTRegular _ expr)) st = do
  (var, st') <- evalExpr expr st
  vs <- toErrorT $ varToTuple st' p var
  tupleAsgnImpl targs vs p st'

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

evalProgram :: Program PPos -> ErrorT IO ()
evalProgram (Prog _ stmts) = dontAllowBreakContinue $
                             dontAllowReturn $
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

-- TODO.
-- usage :: IO ()
-- usage = do
  -- putStrLn $ unlines
    -- [ "usage: Call with one of the following argument combinations:"
    -- , "  --help          Display this help message."
    -- , "  (no arguments)  Parse stdin verbosely."
    -- , "  (files)         Parse content of files verbosely."
    -- , "  -s (files)      Silent mode. Parse content of files silently."
    -- ]
  -- exitFailure



main :: IO ()
main = do -- TODO: Support actual arugments.
  -- putStr $ "Enter a number... "
  -- hFlush stdout
  -- n <- (readIO :: String -> IO Int) =<< getLine
  -- putStrLn $ "Got: " ++ show n
  -- hFlush stdout
  -- putStr $ "Enter anohter number..."
  -- hFlush stdout
  -- n' <- (readIO :: String -> IO Int) =<< getLine
  -- putStrLn $ "Got: " ++ show n'
  -- hFlush stdout

  args <- getArgs
  case args of
    [] -> getContents >>= run "*stdin*"
    f:_ -> readFile f >>= run f
