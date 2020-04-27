module Static where

import AbsLanguage
import Common
import Error
import Parser
import State

shouldBe :: (State -> PPos -> Var -> Error a) -> Expr PPos -> State -> Error ()
shouldBe asF expr st = do
  v <- staticChkExpr expr st
  _ <- asF st (getPos expr) v
  return ()

staticChkBinExpr :: (State -> PPos -> Var -> Error a) -> TypeId -> Expr PPos -> Expr PPos
                 -> State -> Error Var
staticChkBinExpr asF tId lhs rhs st = do
  shouldBe asInt lhs st
  shouldBe asInt rhs st
  return $ defaultVarOfType st tId

staticChkEqExpr :: Expr PPos -> Expr PPos -> State -> Error Var
staticChkEqExpr = undefined

-- TODO: The trick is that evaluating expressions can't change the state in any
-- meaningfull way beacuse single expresion can't change a type or create a new one.
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

staticChkExpr (EEq _ lhs rhs) st = undefined
staticChkExpr (ENeq _ lhs rhs) st = undefined

staticChkExpr (EFnCall _ (Ident name) invokeL) st = undefined -- TODO: Check arg types and
                                                              -- return a sample of
                                                              -- returned type value.
staticChkExpr (EScan _ types) st = undefined
staticChkExpr (EIife _ funDecl invokeL) st = undefined -- TODO: Like fnCall
staticChkExpr (ELValue _ lValue) st = undefined
staticChkExpr (ENew _ (Ident name)) st = undefined
staticChkExpr (EString _ _) st = return $ defaultVarOfType st stringT
staticChkExpr (EInt _ _) st = return $ defaultVarOfType st intT
staticChkExpr (EBool _ _) st = return $ defaultVarOfType st boolT

staticChkStmt :: Stmt PPos -> State -> Error State
staticChkStmt (SIf _ expr stmt) st = undefined
staticChkStmt (SIfElse _ expr stmtIf stmtElse) st = undefined
staticChkStmt (SFor _ (Ident name) exprB exprE stmt) st = undefined
staticChkStmt (SWhile _ expr stmt) st = undefined
staticChkStmt (SExpr _ expr) st = undefined
staticChkStmt (SVDecl _ varDecl) st = undefined
staticChkStmt (SFDecl _ (Ident name) funDecl) st = undefined
staticChkStmt (SSDecl _ (Ident name) strcDecl) st = undefined
staticChkStmt (STDecl _ tupleTarg exprOrTuple) st = undefined
staticChkStmt (SAssign _ lValue expr) st = undefined
staticChkStmt (STAssign _ tupleTarg exprOrTuple) st = undefined
staticChkStmt (SIgnore _ exprOrTuple) st = undefined
staticChkStmt (SReturn _ retExpr) st = undefined
staticChkStmt (SBreak a) st = undefined
staticChkStmt (SCont a) st = undefined
staticChkStmt (SAssert _ expr) st = undefined
staticChkStmt (SPrint _ exprs) st = undefined
staticChkStmt (SBlock _ bind stmts) st = undefined
