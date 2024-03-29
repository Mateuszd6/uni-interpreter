module Static (staticChckProgram) where

-- Static type checking module. It is very simplified run over program that
-- focuses on only finding type errors (and sometimes others, when something
-- must be checked in order to get the type info). It is completely separate
-- from Eval module. Neither Eval nor Static modules include the other. Static
-- type checking can be turned off in the Main.hs file. The interpreter should
-- produce _exactly_ the same errors without it. In disabled, only
-- '12_static_typecheck*.prg' tests should fail (because they demonstrate, that
-- first line of the code is not evaluated with an assert).

import Control.Monad (foldM, foldM_, (<=<))
import qualified Data.Set as Set

import AbsLanguage
import Common
import Error
import Parser
import State

staticChckProgram :: Program PPos -> Error ()
staticChckProgram (Prog _ stmts) = foldM_ (flip staticChkStmt) initialState stmts

defaultReturnType :: FRetT -> State -> Var
defaultReturnType (FRetTSinge tId) st = defaultVarOfType st tId
defaultReturnType (FRetTTuple tIds) st = VTuple $ map (defaultVarOfType st) tIds

-- Like 'scope', but in Error monad.
chkScope :: (State -> Error State) -> Maybe FRetT -> Maybe (Set.Set VarId) -> State
           -> Error State
chkScope = scopeA id (\_ x -> x)

getLValue :: LValue PPos -> State -> Error Var
getLValue (LValueVar p (Ident vname)) st = snd <$> getVar vname p st
getLValue lv@LValueMemb {} st = do
  (members, (_, var)) <- lvalueMem lv st
  structVar <- asStruct st (getPos lv) var
  getStructField structVar members (getPos lv) st

shouldBe :: (State -> PPos -> Var -> Error a) -> Expr PPos -> State -> Error ()
shouldBe asF expr st = do
  v <- staticChkExpr expr st
  _ <- asF st (getPos expr) v
  return ()

staticChkBinExpr :: (State -> PPos -> Var -> Error a) -> TypeId -> Expr PPos -> Expr PPos
                 -> State -> Error Var
staticChkBinExpr asF tId lhs rhs st = do
  shouldBe asF lhs st
  shouldBe asF rhs st
  return $ defaultVarOfType st tId

staticChkFnCallImpl :: Func -> InvokeExprList PPos -> PPos -> State -> Error Var
staticChkFnCallImpl func invokeL p st = do
  let invokeParams = case invokeL of
                       IELDefault _ exprs -> exprs
                       IELEmpty _ -> []
  enforceParamLengthEqual p (length $ funcParams func) (length invokeParams)

  -- Check params
  mapM_ (\(t, e) -> staticChkExpr e st >>= \v -> enforceType v t (getPos e) st) $
    zip (map thrd3 $ funcParams func) invokeParams

  return $ defaultReturnType (funcRetT func) st

staticChkFnBody :: Stmt PPos -> [Param] -> FRetT -> PPos -> State -> Error ()
staticChkFnBody body fParams retT p st = do
  let addParam param s = snd <$> createVar (fst3 param) False
                          (defaultVarOfType st (thrd3 param)) p s
  _ <- chkScope (staticChkStmt body) (Just retT) Nothing =<<
       foldM (flip addParam) st fParams
  return ()

staticChkEqExpr :: Expr PPos -> Expr PPos -> State -> Error Var
staticChkEqExpr lhs rhs st =
  let canComp :: Var -> Var -> Error Bool
      canComp (VInt x) (VInt y) = return $ x == y
      canComp (VBool x) (VBool y) = return $ x == y
      canComp (VString x) (VString y) = return $ x == y
      canComp v1 v2 = Fail $ EDCantCompare (getPos lhs)
                                           (getTypeName (varTypeId v1) st)
                                           (getTypeName (varTypeId v2) st)
  in do
    v1 <- staticChkExpr lhs st
    v2 <- staticChkExpr rhs st
    _ <- canComp v1 v2
    return $ defaultVarOfType st boolT

-- The trick is that evaluating expressions can't change the state in any
-- meaningfull way to the type checker beacuse single expresion can't change a
-- var/func/type or create a new one.
staticChkExpr :: Expr PPos -> State -> Error Var
staticChkExpr (ELor _ lhs rhs) st = staticChkBinExpr asBool boolT lhs rhs st
staticChkExpr (ELand _ lhs rhs) st = staticChkBinExpr asBool boolT lhs rhs st
staticChkExpr (EXor _ lhs rhs) st = staticChkBinExpr asBool boolT lhs rhs st
staticChkExpr (EGeq _ lhs rhs) st = staticChkBinExpr asInt boolT lhs rhs st
staticChkExpr (ELeq _ lhs rhs) st = staticChkBinExpr asInt boolT lhs rhs st
staticChkExpr (EGt _ lhs rhs) st = staticChkBinExpr asInt boolT lhs rhs st
staticChkExpr (ELt _ lhs rhs) st = staticChkBinExpr asInt boolT lhs rhs st
staticChkExpr (EPlus _ lhs rhs) st = staticChkBinExpr asInt intT lhs rhs st
staticChkExpr (EMinus _ lhs rhs) st = staticChkBinExpr asInt intT lhs rhs st
staticChkExpr (ECat _ lhs rhs) st = staticChkBinExpr asString stringT lhs rhs st
staticChkExpr (ETimes _ lhs rhs) st = staticChkBinExpr asInt intT lhs rhs st
staticChkExpr (EDiv _ lhs rhs) st = staticChkBinExpr asInt intT lhs rhs st
staticChkExpr (EMod _ lhs rhs) st = staticChkBinExpr asInt intT lhs rhs st
staticChkExpr (EPow _ lhs rhs) st = staticChkBinExpr asInt intT lhs rhs st

staticChkExpr (EEq _ lhs rhs) st = staticChkEqExpr lhs rhs st
staticChkExpr (ENeq _ lhs rhs) st = staticChkEqExpr lhs rhs st

staticChkExpr (EFnCall p (Ident name) invokeL) st = do
  func <- snd <$> getFunc name p st
  staticChkFnCallImpl func invokeL p st

staticChkExpr (EScan _ types) st = VTuple . (defaultVarOfType st intT :) <$>
  mapM (\t -> defaultVarOfType st <$> getTypeId t st) types

staticChkExpr (EIife p (FDDefault _ params _ funRet stmts) invokeL) st = do
  retT <- parseRetType funRet st
  fParams <- funcToParams params st
  let body = SBlock p (BdNone Nothing) stmts
      func = Func (-1) body retT fParams Nothing (stateScope st) (scopeCnt st)

  staticChkFnBody body fParams retT p st
  staticChkFnCallImpl func invokeL p st

staticChkExpr (ELValue _ lValue) st = getLValue lValue st
staticChkExpr (ENew p (Ident name) asgnFields) st = do
  let fields = asgnFieldsToList asgnFields
  tId <- fst <$> getTypeStruct name p st
  enforceNotParamRepeated (map fst fields) (flip EDStructArgRepeated p)
  mapM_ (\(field, expr) -> do
            destTypeId <- getStructFieldType tId field p st
            var <- staticChkExpr expr st
            enforceType var destTypeId p st) fields

  defaultVarOfType st <$> getTypeId (TUser p (Ident name)) st

staticChkExpr (EString _ _) st = return $ defaultVarOfType st stringT
staticChkExpr (EInt _ _) st = return $ defaultVarOfType st intT
staticChkExpr (EBool _ _) st = return $ defaultVarOfType st boolT

staticChkStmt :: Stmt PPos -> State -> Error State
staticChkStmt (SIf _ expr stmt) st = do
  shouldBe asBool expr st
  _ <- staticChkStmt stmt st
  return st

staticChkStmt (SIfElse _ expr stmtIf stmtElse) st = do
  shouldBe asBool expr st
  _ <- staticChkStmt stmtIf st
  _ <- staticChkStmt stmtElse st
  return st -- The return state does not matter, both if and else are scoped.

staticChkStmt (SFor p (Ident name) exprB exprE stmt) st = do
  shouldBe asInt exprB st
  shouldBe asInt exprE st
  chkScope ((staticChkStmt stmt . snd) <=< createVar name False (VInt 0) p)
             Nothing Nothing st

staticChkStmt (SWhile _ expr stmt) st = do
  shouldBe asBool expr st
  staticChkStmt stmt st

staticChkStmt (SExpr _ expr) st = staticChkExpr expr st >> return st

staticChkStmt (SVDecl p (DVDecl _ (Ident name) _ tp)) st = do
  tId <- getTypeId tp st
  snd <$> createVar name False (defaultVarOfType st tId) p st

staticChkStmt (SVDecl p (DVDeclAsgn _ (Ident name) _ tp expr)) st = do
  tId <- getTypeId tp st
  v <- staticChkExpr expr st
  enforceType v tId (getPos expr) st
  snd <$> createVar name False v p st

staticChkStmt (SVDecl p (DVDeclDeduce _ (Ident name) _ expr)) st = do
  v <- staticChkExpr expr st
  snd <$> createVar name False v p st

staticChkStmt (SFDecl p (Ident fname) (FDDefault _ params _ funRet stmts)) st = do
  retT <- parseRetType funRet st
  fParams <- funcToParams params st
  enforceNotParamRepeated (map fst3 fParams) (flip EDFuncArgRepeated p)
  let body = SBlock p (BdNone Nothing) stmts
  st' <- snd <$> createFunc fname body fParams retT Nothing p st
  _ <- chkScope (\s -> staticChkFnBody body fParams retT p s >> return s)
                  Nothing Nothing st' -- eval in st' to allow recursion
  return st'

staticChkStmt (SSDecl p (Ident sname) (SDDefault _ members)) st = do
  strMembs <- getStructMemebers members st
  snd <$> createStruct sname p strMembs st


staticChkStmt (STDecl p (TTar _ idents) (EOTRegular _ expr)) st = do
  exprVar <- staticChkExpr expr st
  case exprVar of
    VTuple vars -> addTupleToStateImpl idents vars p st
    _ -> Fail $ EDTypeError "tuple" (getTypeName (varTypeId exprVar) st) p

staticChkStmt (STDecl p (TTar _ idents) (EOTTuple _ exprs)) st = do
  vars <- mapM (`staticChkExpr` st) exprs
  addTupleToStateImpl idents vars p st

staticChkStmt (SAssign p lv expr) st = do
  asgnVal <- staticChkExpr expr st
  enforceVarIsNotVoid asgnVal p
  (membs, (_, var)) <- lvalueMem lv st

  case membs of
    [] -> enforceType var (varTypeId asgnVal) p st
    _ -> do -- Assign field of a struct.
      structTId <- fst <$> asStruct st (getPos lv) var
      staticChkFieldAsgn membs structTId asgnVal p st
  return st

staticChkStmt (STAssign p (TTar _ idents) (EOTRegular _ expr)) st = do
  vars <- staticChkExpr expr st >>= asTuple st p
  staticChkBulkAsgn idents vars p st
  return st

staticChkStmt (STAssign p (TTar _ idents) (EOTTuple _ exprs)) st = do
  vars <- mapM (`staticChkExpr` st) exprs
  staticChkBulkAsgn idents vars p st
  return st

staticChkStmt (SIgnore _ (EOTRegular _ expr)) st = staticChkExpr expr st >> return st

staticChkStmt (SIgnore _ (EOTTuple _ exprs)) st =
  mapM_ (`staticChkExpr` st) exprs >> return st

staticChkStmt (SReturn p retExpr) st = do
  v <- staticChkReturnExpr retExpr st
  maybe (Ok ()) (\r -> enforceReturnIsCorret v r p st) (currRetT st)
  return st

staticChkStmt (SBreak _) st = return st
staticChkStmt (SCont _) st = return st
staticChkStmt (SAssert _ expr) st = shouldBe asBool expr st >> return st
staticChkStmt (SPrint _ exprs) st = mapM_ (`staticChkExpr` st) exprs >> return st
staticChkStmt (SBlock _ _ stmts) st = chkScope (flip (foldM (flip staticChkStmt)) stmts)
                                      Nothing Nothing st

staticChkReturnExpr :: ReturnExpr PPos -> State -> Error Var
staticChkReturnExpr (RExNone _) _ = return VEmpty
staticChkReturnExpr (RExRegular _ (EOTRegular _ expr)) st = staticChkExpr expr st
staticChkReturnExpr (RExRegular _ (EOTTuple _ exprs)) st = VTuple <$>
  mapM (`staticChkExpr` st) exprs

staticChkFieldAsgn :: [String] -> TypeId -> Var -> PPos -> State -> Error ()
staticChkFieldAsgn (m:ms) structTId var p st = do
  tId <- getStructFieldType structTId m p st
  case ms of
    [] -> enforceType var tId p st
    _ -> staticChkFieldAsgn ms tId var p st
staticChkFieldAsgn [] _ _ _ _ = undefined -- Can't reach

staticChkBulkAsgn :: [IdentOrIgnr PPos] -> [Var] -> PPos -> State -> Error ()
staticChkBulkAsgn idents vars p st = do
  zipped <- errorFromMaybe (EDTupleNumbersDontMatch p (length idents) (length vars)) $
            tryZip idents vars
  mapM_ (\(tar, v) -> case tar of
                        IOIIdent p' (Ident vname) -> do
                          var <- snd <$> getVar vname p' st
                          enforceType var (varTypeId v) p' st
                        IOIIgnore _ -> return ())
        zipped

addTupleToStateImpl :: [IdentOrIgnr PPos] -> [Var] -> PPos -> State -> Error State
addTupleToStateImpl idents vars p st = do
  zipped <- errorFromMaybe (EDTupleNumbersDontMatch p (length idents) (length vars)) $
            tryZip idents vars

  foldrM (\(tar, v) s ->
             case tar of
               IOIIgnore _ -> return s
               IOIIdent pv (Ident name) -> snd <$> createVar name False v pv s)
         st zipped
