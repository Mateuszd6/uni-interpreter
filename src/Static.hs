module Static where

import Control.Monad (foldM, foldM_, when, (<=<))
import qualified Data.Set as Set

import AbsLanguage
import Common
import Error
import Parser
import State

-- EOTRegular a (Expr a)
-- EOTTuple a [Expr a]

staticChckProgram :: Program PPos -> Error ()
staticChckProgram (Prog _ stmts) = foldM_ (flip staticChkStmt) initialState stmts

checkScope :: (State -> Error State) -> Maybe (Set.Set VarId) -> State
           -> Error State
checkScope fun bind st =
  let newBind = case bind of
                  Nothing -> bindVars st
                  Just set -> (scopeCnt st + 1, set) : bindVars st
  in do
    st' <- fun st { scopeCnt = scopeCnt st + 1, bindVars = newBind }
    return $ st'{ scopeCnt = scopeCnt st,
                  stateScope = stateScope st,
                  bindVars = bindVars st }
    -- Rollbacks scope and bind vars after leaving the scope.

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

-- TODO: Decide if it lives or not.
-- checkExprList :: (Var -> Bool) -> (Expr PPos -> Var -> ErrorDetail) -> [Expr PPos] -> State
              -- -> Error ()
-- checkExprList checkFun edFun exprs st =
  -- let check expr = staticChkExpr expr st >>= \v -> when (checkFun v) $ Fail $ edFun expr v
  -- in
    -- mapM_ check exprs

staticChkBinExpr :: (State -> PPos -> Var -> Error a) -> TypeId -> Expr PPos -> Expr PPos
                 -> State -> Error Var
staticChkBinExpr asF tId lhs rhs st = do
  shouldBe asF lhs st
  shouldBe asF rhs st
  return $ defaultVarOfType st tId

staticChkEqExpr :: Expr PPos -> Expr PPos -> State -> Error Var
staticChkEqExpr lhs rhs st =
  -- TODO: rename to enforceComprarable and move somewhere merge with eval.
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

-- TODO: The trick is that evaluating expressions can't change the state in any
-- meaningfull way beacuse single expresion can't change a type or create a new
-- one.
staticChkExpr :: Expr PPos -> State -> Error Var
staticChkExpr (ELor _ lhs rhs) st = staticChkBinExpr asBool boolT lhs rhs st
staticChkExpr (ELand _ lhs rhs) st = staticChkBinExpr asBool boolT lhs rhs st
staticChkExpr (EXor _ lhs rhs) st = staticChkBinExpr asBool boolT lhs rhs st
staticChkExpr (EGeq _ lhs rhs) st = staticChkBinExpr asInt boolT lhs rhs st
staticChkExpr (ELeq _ lhs rhs) st = staticChkBinExpr asBool boolT lhs rhs st
staticChkExpr (EGt _ lhs rhs) st = staticChkBinExpr asBool boolT lhs rhs st
staticChkExpr (ELt _ lhs rhs) st = staticChkBinExpr asBool boolT lhs rhs st
staticChkExpr (EPlus _ lhs rhs) st = staticChkBinExpr asInt intT lhs rhs st
staticChkExpr (EMinus _ lhs rhs) st = staticChkBinExpr asInt intT lhs rhs st
staticChkExpr (ECat _ lhs rhs) st = staticChkBinExpr asString stringT lhs rhs st
staticChkExpr (ETimes _ lhs rhs) st = staticChkBinExpr asInt intT lhs rhs st
staticChkExpr (EDiv _ lhs rhs) st = staticChkBinExpr asInt intT lhs rhs st
staticChkExpr (EMod _ lhs rhs) st = staticChkBinExpr asInt intT lhs rhs st
staticChkExpr (EPow _ lhs rhs) st = staticChkBinExpr asInt intT lhs rhs st

staticChkExpr (EEq _ lhs rhs) st = staticChkEqExpr lhs rhs st
staticChkExpr (ENeq _ lhs rhs) st = staticChkEqExpr lhs rhs st

-- TODO: Check arg types and return a sample of returned type value.
staticChkExpr (EFnCall _ (Ident name) invokeL) st = undefined
staticChkExpr (EScan _ types) st = undefined
staticChkExpr (EIife _ funDecl invokeL) st = undefined -- TODO: Like fnCall
staticChkExpr (ELValue _ lValue) st = getLValue lValue st
staticChkExpr (ENew _ (Ident name)) st = undefined
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
  checkScope ((staticChkStmt stmt . snd) <=< createVar name False (VInt 0) p)
             Nothing st

staticChkStmt (SWhile _ expr stmt) st = do
  shouldBe asBool expr st
  staticChkStmt stmt st

staticChkStmt (SExpr _ expr) st = do
  staticChkExpr expr st
  return st

staticChkStmt (SVDecl p (DVDecl _ (Ident name) _ tp)) st = do
  tId <- getTypeId tp st
  snd <$> createVar name False (defaultVarOfType st tId) p st

staticChkStmt (SVDecl p (DVDeclAsgn _ (Ident name) _ tp expr)) st = do
  tId <- getTypeId tp st
  v <- staticChkExpr expr st
  enforceType v tId p st
  snd <$> createVar name False v p st

staticChkStmt (SVDecl p (DVDeclDeduce _ (Ident name) _ expr)) st = do
  v <- staticChkExpr expr st
  snd <$> createVar name False v p st

staticChkStmt (SFDecl p (Ident fname) (FDDefault _ params _ funRet stmts)) st = do
  retT <- parseRetType funRet st
  fParams <- funcToParams params st
  -- bindV <- toCtrlT $ getBindVars bd st -- TODO: Decide if we support bind or not!
  let addParam param st = snd <$> createVar (fst3 param) False
                                    (defaultVarOfType st (thrd3 param)) p st
      body = SBlock p (BdNone Nothing) stmts
  st' <- foldM (\s x -> addParam x s) st fParams
  staticChkStmt body st -- TODO: add args here.
  snd <$> createFunc fname body fParams retT Nothing p st

staticChkStmt (SSDecl p (Ident sname) (SDDefault _ members)) st = do
  strMembs <- getStructMemebers members st
  snd <$> createStruct sname p strMembs st

staticChkStmt (STDecl _ tupleTarg exprOrTuple) st = return st -- TODO
staticChkStmt (SAssign _ lValue expr) st = return st -- TODO
staticChkStmt (STAssign _ tupleTarg exprOrTuple) st = return st -- TODO
staticChkStmt (SIgnore _ exprOrTuple) st = return st -- TODO: Must evauate expr, because it can be an iife!
staticChkStmt (SReturn _ retExpr) st = return st -- TODO
staticChkStmt (SBreak a) st = return st
staticChkStmt (SCont a) st = return st
staticChkStmt (SAssert _ expr) st = shouldBe asBool expr st >> return st
staticChkStmt (SPrint _ exprs) st = return st -- TODO: Assert that elems are printable? Don't allow void?
staticChkStmt (SBlock _ bind stmts) st = checkScope (flip (foldM (flip staticChkStmt)) stmts) Nothing st
