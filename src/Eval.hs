{-# LANGUAGE TupleSections #-}

module Eval where

import Control.Monad (foldM, foldM_, unless)
import Control.Monad.Trans.Class (lift, MonadTrans(..))
import Data.Bits (xor)
import Data.Foldable (foldl')
import System.IO (hFlush, stdout)
import System.IO.Error (catchIOError)
import Text.Read (readMaybe)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

import AbsLanguage
import Common
import Error
import Parser
import State

-- Abstract commonly occurinng pattern of evaluating var and then checking its type.
-- asF functions are defined in the State module (asInt, asBool, ...)
exprAs :: (State -> PPos -> Var -> Error a) -> Expr PPos -> State -> CtrlT IO (a, State)
exprAs asF expr st = do
  (v, st') <- evalExpr expr st
  x <- toCtrlT $ asF st (getPos expr) v
  return (x, st')

-- Evaluates binary expresion with parametrized the func.
evalBinExpr :: (State -> PPos -> Var -> Error a) -> -- Convert Var to desired type.
               (a -> a -> r) -> -- Func performed on wrapped value.
               (r -> Var) -> -- Ctor that wraps computed value back to Var.
               Expr PPos -> -- LHS expression.
               Expr PPos -> -- RHS expression.
               Maybe (a -> a -> PPos -> Error ()) -> -- Constraints for the vars.
               State ->
               CtrlT IO (Var, State)
evalBinExpr varTo func varCtor lhs rhs constr st = do
  -- The *Conv variables are unwraped value from vars with desired type.
  (evaledL, st') <- evalExpr lhs st
  evaledLConv <- toCtrlT $ varTo st (getPos lhs) evaledL

  (evaledR, st'') <- evalExpr rhs st'
  evaledRConv <- toCtrlT $ varTo st (getPos rhs) evaledR

  toCtrlT $ case constr of -- If constrain fucn is given eval it.
    Just f -> f evaledLConv evaledRConv $ getPos rhs
    Nothing -> return ()

  -- Now use the ctor to wrap calculated value back into Var type.
  return (varCtor $ evaledLConv `func` evaledRConv, st'')

evalEqualExpr :: Bool -> Expr PPos -> Expr PPos -> State -> CtrlT IO (Var, State)
evalEqualExpr neg lhs rhs st =
  let tryComp :: Var -> Var -> Error Bool
      tryComp (VInt x) (VInt y) = return $ x == y
      tryComp (VBool x) (VBool y) = return $ x == y
      tryComp (VString x) (VString y) = return $ x == y
      tryComp v1 v2 = Fail $ EDCantCompare (getPos lhs)
                                           (getTypeName (varTypeId v1) st)
                                           (getTypeName (varTypeId v2) st)
  in do
    (evaledL, st') <- evalExpr lhs st
    (evaledR, st'') <- evalExpr rhs st'
    comp <- toCtrlT $ tryComp evaledL evaledR
    return (VBool $ (if neg then not else id) comp, st'')

-- Evalulate a list of expressions with foldr, return the list and a new
-- state. Each expression is evaluated in a new state (right to left).
evalExprsListr :: [Expr PPos] -> State -> CtrlT IO ([Var], State)
evalExprsListr exprs st = do
  (vars, st') <- foldrM (\ex (vars, s) -> appendFst vars <$> evalExpr ex s) ([], st) exprs
  toCtrlT $ mapM_ (\(e, v) -> case v of
                                 VTuple _ -> Fail $ EDTupleNotAllowed (getPos e)
                                 _ -> return ())
    $ zip exprs vars
  return (vars, st')

fnCallParams :: InvokeExprList PPos -> State -> CtrlT IO ([Var], State)
fnCallParams (IELEmpty _) st = return ([], st)
fnCallParams (IELDefault _ exprs) st = evalExprsListr exprs st

-- Value is returned in a tricky way through 'Flow', so it has to be catched.
evalFunction :: Func -> [Var] -> PPos -> State -> CtrlT IO State
evalFunction func invokeP p st = do
  let foo = case funcBind func of
        Just x -> (scopeCnt st + 1, x) : bindVars st
        Nothing -> bindVars st

  st' <- toCtrlT $
    foldrM (\(par, (pname, spec, tId)) s -> enforceType par tId p s >>
             snd <$> createVar pname (varSpecReadOnly spec) par p s)
           st { scopeCnt = scopeCnt st + 1,
                stateScope = funcScope func,
                bindVars = foo } $
           zip invokeP (funcParams func)

  evalStmt (funcBody func) st'

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
getBindVars (BdPure _) _ = return $ Just $ Set.fromList []
getBindVars (BdPureAlt _) _ = return $ Just $ Set.fromList[]
getBindVars (BdNone _) _ = return Nothing
getBindVars (BdDefault p idents) st = fromMaybeList <$>
  foldM (\acc name -> (\x -> (:) x <$> acc) . fst <$> getVar name p st)
        (Just [])
        (map (\(Ident str) -> str) idents)
  where
    fromMaybeList :: Ord a => Maybe [a] -> Maybe (Set.Set a)
    fromMaybeList (Just l) = Just $ Set.fromList l
    fromMaybeList Nothing = Nothing

returnHanlder :: Func -> PPos -> (CtrlT IO State -> CtrlT IO (Var, State))
returnHanlder func p =
  returnHndl p . dontAllowBreakContinue
  where
    returnHndl = if not $ funcReturnsVoid $ funcRetT func
                   then expectReturnValue (funcRetT func)
                   else catchReturnVoid

evalExpr :: Expr PPos -> State -> CtrlT IO (Var, State)

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

evalExpr (ELValue p (LValueVar _ (Ident vname))) st = toCtrlT $
  (, st) . snd <$> getVar vname p st

evalExpr (ELValue p lv@LValueMemb {}) st = do
  (members, (_, var)) <- toCtrlT $ lvalueMem lv st
  structVar <- toCtrlT $ asStruct st p var
  toCtrlT $ (, st) <$> getStructField structVar members p st

evalExpr (ENew p (Ident name)) st = do
  v <- toCtrlT $ defaultVarOfType st . fst <$> getTypeStruct name p st
  return (v, st)

evalExpr (EFnCall p (Ident fname) params) st = do
  func <- toCtrlT $ snd <$> getFunc fname p st
  (invokeParams, st') <- fnCallParams params st

  toCtrlT $ enforceParamLengthEqual p (length $ funcParams func) (length invokeParams)
  toCtrlT $ enforceFnCallBindRules fname func p st

  scope2 (returnHanlder func p . evalFunction func invokeParams p) Nothing st'

evalExpr (EIife p (FDDefault _ params bind funRet stmts) invkParams) st = do
  retT <- toCtrlT $ parseRetType funRet st
  fParams <- toCtrlT $ funcToParams params st
  (invokeParams, st') <- fnCallParams invkParams st
  bindV <- toCtrlT $ getBindVars bind st'

  let body = SBlock p (BdNone Nothing) stmts
      func = Func (-1) body retT fParams bindV (stateScope st') (scopeCnt st')

  toCtrlT $ enforceParamLengthEqual p (length $ funcParams func) (length invokeParams)
  -- No need to enforce bind rules, as iife is defined in the scope its used.

  scope2 (returnHanlder func p . evalFunction func invokeParams p) Nothing st'

evalExpr (EScan _ types) st = do
  toCtrlT $ mapM_ enforceIsBultinType types
  line <- lift $ catchIOError getLine (\_ -> return [])
  let failedParse t l = (False, (False, defaultVal t) : l)
      vars = reverse $ snd $
             foldl' (\(ok, l) (t, s) ->
                        if ok
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
    parse _ _ = undefined -- We've checked for these types before

    defaultVal :: Type PPos -> Var
    defaultVal t = defaultVarOfType st $ nofail $ getTypeId t st

varSpecReadOnly :: VarSpec a -> Bool
varSpecReadOnly (VSReadOnly _) = True
varSpecReadOnly _ = False

evalVarDeclImpl :: VarSpec PPos -> String -> Var -> PPos -> State -> Error State
evalVarDeclImpl spec vname var p st = do
  enforceVarIsNotVoid var p
  snd <$> createVar vname (varSpecReadOnly spec) var p st

evalVarAsgnImpl :: String -> Var -> PPos -> State -> Error State
evalVarAsgnImpl vname asgnVal p st = do
  (vId, var) <- getVar vname p st
  enforceType var (varTypeId asgnVal) p st
  setVar vId asgnVal p st

evalVarDecl :: VarDecl PPos -> State -> CtrlT IO State
evalVarDecl (DVDecl p (Ident vname) spec tp) st = do
  tId <- toCtrlT $ getTypeId tp st
  toCtrlT $ evalVarDeclImpl spec vname (defaultVarOfType st tId) p st

evalVarDecl (DVDeclAsgn p (Ident vname) spec tp expr) st = do
  (v, st') <- evalExpr expr st
  tId <- toCtrlT $ getTypeId tp st'
  toCtrlT $ enforceType v tId (getPos expr) st'
  toCtrlT $ evalVarDeclImpl spec vname v p st'

evalVarDecl (DVDeclDeduce p (Ident vname) spec expr) st = do
    (v, st') <- evalExpr expr st
    toCtrlT $ evalVarDeclImpl spec vname v p st'

evalIfStmtImpl :: Expr PPos -> Stmt PPos -> Maybe (Stmt PPos) -> State -> CtrlT IO State
evalIfStmtImpl expr stmt elseStmt st = do
  (cond, st') <- exprAs asBool expr st
  lift $ putStrLn $ "evaluated bool: " ++ show cond
  if cond
    then evalStmt stmt st'
    else case elseStmt of
           Just s -> evalStmt s st'
           Nothing -> return st'

tupleAsgnOrDeclImpl :: Bool -> [IdentOrIgnr PPos] -> [Var] -> PPos -> State
                    -> CtrlT IO State
tupleAsgnOrDeclImpl decl targs vs p st = do
  zipped <- toCtrlT $
            errorFromMaybe (EDTupleNumbersDontMatch p (length targs) (length vs)) $
            tryZip targs vs

  let action = if decl then evalVarDeclImpl (VSNone Nothing) else evalVarAsgnImpl
  toCtrlT $ foldrM (\(tar, v) s ->
                       case tar of
                         IOIIgnore _ -> return s
                         IOIIdent pv (Ident name) -> action name v pv s)
                    st zipped

tupleDeclImpl :: [IdentOrIgnr PPos] -> [Var] -> PPos -> State -> CtrlT IO State
tupleDeclImpl = tupleAsgnOrDeclImpl True

tupleAsgnImpl :: [IdentOrIgnr PPos] -> [Var] -> PPos -> State -> CtrlT IO State
tupleAsgnImpl = tupleAsgnOrDeclImpl False

evalStmt :: Stmt PPos -> State -> CtrlT IO State

evalStmt (SBlock _ bind stmts) st = do
  bindV <- toCtrlT $ getBindVars bind st
  scope (flip (foldM (flip evalStmt)) stmts) bindV st

evalStmt (SAssert p expr) st = do
  (cond, st') <- exprAs asBool expr st
  toCtrlT $ unless cond $ Fail $ EDAssertFail p
  return st'

evalStmt (SPrint _ exprs) st = do
  st' <- foldM (\s ex -> evalExpr ex s >>= lift . printFstRetSnd) st exprs
  lift $ hFlush stdout -- Just in case
  return st'

evalStmt (SIf _ expr stmt) st = scope (evalIfStmtImpl expr stmt Nothing) Nothing st
evalStmt (SIfElse _ expr stmt elStmt) st =
  scope (evalIfStmtImpl expr stmt (Just elStmt)) Nothing st

evalStmt (SWhile _ expr_ stmt_) st_ = scope (catchBreak . evalWhileImpl expr_ stmt_)
                                      Nothing st_
  where
    evalWhileImpl :: Expr PPos -> Stmt PPos -> State -> CtrlT IO State
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
        s' <- toCtrlT $ evalVarDeclImpl (VSReadOnly p) iterName (VInt i) (getPos e) s
        catchContinue $ evalStmt stmt s'
      loopBody s = catchBreak $ foldM (\acc i -> scope (loopStep i) Nothing acc) s iters

  scope loopBody Nothing st''

-- Discard expression result and return new state.
evalStmt (SExpr _ expr) st = snd <$> evalExpr expr st

evalStmt (SVDecl _ vdecl) st = evalVarDecl vdecl st

evalStmt (SFDecl p (Ident fname) (FDDefault _ params bd funRet stmts)) st = do
  lift $ putStrLn $ "Declaring function named `" ++ fname ++ "'."
  retT <- toCtrlT $ parseRetType funRet st
  fParams <- toCtrlT $ funcToParams params st
  bindV <- toCtrlT $ getBindVars bd st
  let body = SBlock p (BdNone Nothing) stmts
  toCtrlT $ snd <$> createFunc fname body fParams retT bindV p st

evalStmt (SSDecl p (Ident sname) (SDDefault _ members)) st = do
  strMembs <- toCtrlT $ getStructMemebers members st
  toCtrlT $ snd <$> createStruct sname p strMembs st

evalStmt (STDecl p (TTar _ targs) (EOTTuple _ exprs)) st = do
  (vs, st') <- evalExprsListr exprs st
  tupleDeclImpl targs vs p st'

evalStmt (STAssign p (TTar _ targs) (EOTTuple _ exprs)) st = do
  (vs, st') <- evalExprsListr exprs st
  tupleAsgnImpl targs vs p st'

evalStmt (SIgnore _ (EOTRegular _ expr)) st = snd <$> evalExpr expr st
evalStmt (SIgnore _ (EOTTuple _ exprs)) st = snd <$> evalExprsListr exprs st

evalStmt (STDecl p (TTar _ targs) (EOTRegular _ expr)) st = do
  (tuple, st') <- exprAs asTuple expr st
  tupleDeclImpl targs tuple p st'

evalStmt (STAssign p (TTar _ targs) (EOTRegular _ expr)) st = do
  (tuple, st') <- exprAs asTuple expr st
  tupleAsgnImpl targs tuple p st'

evalStmt (SAssign p lv expr) st = do
  (asgnVal, st') <- evalExpr expr st
  toCtrlT $ enforceVarIsNotVoid asgnVal p
  (membs, (vId, var)) <- toCtrlT $ lvalueMem lv st'

  toCtrlT $ case membs of
    [] -> do -- Assign single variable.
      enforceType var (varTypeId asgnVal) p st'
      setVar vId asgnVal p st'
    _ -> do -- Assign field of a struct.
      (tId, struct) <- asStruct st' (getPos lv) var
      newStruct <- assgnStructField (tId, struct) membs asgnVal p st'
      setVar vId (VStruct tId newStruct) p st'

evalStmt (SReturn p (RExNone _)) st = throw (ExReturn p VEmpty) st
evalStmt (SReturn p (RExRegular _ (EOTRegular _ expr))) st = do
  (result, st') <- evalExpr expr st
  throw (ExReturn p result) st'

evalStmt (SReturn p (RExRegular _ (EOTTuple _ exprs))) st = do
  (result, st') <- evalExprsListr exprs st
  throw (ExReturn p (VTuple result)) st'

evalStmt (SBreak p) st = throw (ExBreak p) st
evalStmt (SCont p) st = throw (ExContinue p) st

evalProgram :: Program PPos -> CtrlT IO ()
evalProgram (Prog _ stmts) = dontAllowBreakContinue $ dontAllowReturn $
                             foldM_ (flip evalStmt) initialState stmts
