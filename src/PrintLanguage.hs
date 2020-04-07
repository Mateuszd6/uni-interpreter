{-# LANGUAGE CPP #-}
#if __GLASGOW_HASKELL__ <= 708
{-# LANGUAGE OverlappingInstances #-}
#endif
{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}

-- | Pretty-printer for PrintLanguage.
--   Generated by the BNF converter.

module PrintLanguage where

import qualified AbsLanguage
import Data.Char

-- | The top-level printing method.

printTree :: Print a => a -> String
printTree = render . prt 0

type Doc = [ShowS] -> [ShowS]

doc :: ShowS -> Doc
doc = (:)

render :: Doc -> String
render d = rend 0 (map ($ "") $ d []) "" where
  rend i ss = case ss of
    "["      :ts -> showChar '[' . rend i ts
    "("      :ts -> showChar '(' . rend i ts
    "{"      :ts -> showChar '{' . new (i+1) . rend (i+1) ts
    "}" : ";":ts -> new (i-1) . space "}" . showChar ';' . new (i-1) . rend (i-1) ts
    "}"      :ts -> new (i-1) . showChar '}' . new (i-1) . rend (i-1) ts
    ";"      :ts -> showChar ';' . new i . rend i ts
    t  : ts@(p:_) | closingOrPunctuation p -> showString t . rend i ts
    t        :ts -> space t . rend i ts
    _            -> id
  new i   = showChar '\n' . replicateS (2*i) (showChar ' ') . dropWhile isSpace
  space t = showString t . (\s -> if null s then "" else ' ':s)

  closingOrPunctuation :: String -> Bool
  closingOrPunctuation [c] = c `elem` closerOrPunct
  closingOrPunctuation _   = False

  closerOrPunct :: String
  closerOrPunct = ")],;"

parenth :: Doc -> Doc
parenth ss = doc (showChar '(') . ss . doc (showChar ')')

concatS :: [ShowS] -> ShowS
concatS = foldr (.) id

concatD :: [Doc] -> Doc
concatD = foldr (.) id

replicateS :: Int -> ShowS -> ShowS
replicateS n f = concatS (replicate n f)

-- | The printer class does the job.

class Print a where
  prt :: Int -> a -> Doc
  prtList :: Int -> [a] -> Doc
  prtList i = concatD . map (prt i)

instance {-# OVERLAPPABLE #-} Print a => Print [a] where
  prt = prtList

instance Print Char where
  prt _ s = doc (showChar '\'' . mkEsc '\'' s . showChar '\'')
  prtList _ s = doc (showChar '"' . concatS (map (mkEsc '"') s) . showChar '"')

mkEsc :: Char -> Char -> ShowS
mkEsc q s = case s of
  _ | s == q -> showChar '\\' . showChar s
  '\\'-> showString "\\\\"
  '\n' -> showString "\\n"
  '\t' -> showString "\\t"
  _ -> showChar s

prPrec :: Int -> Int -> Doc -> Doc
prPrec i j = if j < i then parenth else id

instance Print Integer where
  prt _ x = doc (shows x)

instance Print Double where
  prt _ x = doc (shows x)

instance Print AbsLanguage.Ident where
  prt _ (AbsLanguage.Ident i) = doc (showString i)
  prtList _ [x] = concatD [prt 0 x]
  prtList _ (x:xs) = concatD [prt 0 x, doc (showString ","), prt 0 xs]

instance Print AbsLanguage.Program where
  prt i e = case e of
    AbsLanguage.Prog stmts -> prPrec i 0 (concatD [prt 0 stmts])

instance Print AbsLanguage.LValue where
  prt i e = case e of
    AbsLanguage.LValueVar id -> prPrec i 0 (concatD [prt 0 id])
    AbsLanguage.LValueMemb lvalue id -> prPrec i 0 (concatD [prt 0 lvalue, doc (showString "."), prt 0 id])

instance Print AbsLanguage.Expr where
  prt i e = case e of
    AbsLanguage.EPlus expr1 expr2 -> prPrec i 0 (concatD [prt 0 expr1, doc (showString "+"), prt 2 expr2])
    AbsLanguage.EMinus expr1 expr2 -> prPrec i 0 (concatD [prt 0 expr1, doc (showString "-"), prt 2 expr2])
    AbsLanguage.ECat expr1 expr2 -> prPrec i 0 (concatD [prt 0 expr1, doc (showString "@"), prt 2 expr2])
    AbsLanguage.ETimes expr1 expr2 -> prPrec i 2 (concatD [prt 2 expr1, doc (showString "*"), prt 3 expr2])
    AbsLanguage.EDiv expr1 expr2 -> prPrec i 2 (concatD [prt 2 expr1, doc (showString "/"), prt 3 expr2])
    AbsLanguage.EPow expr1 expr2 -> prPrec i 3 (concatD [prt 3 expr1, doc (showString "^"), prt 4 expr2])
    AbsLanguage.EEq expr1 expr2 -> prPrec i 4 (concatD [prt 4 expr1, doc (showString "=="), prt 5 expr2])
    AbsLanguage.ENeq expr1 expr2 -> prPrec i 4 (concatD [prt 4 expr1, doc (showString "!="), prt 5 expr2])
    AbsLanguage.EGeq expr1 expr2 -> prPrec i 4 (concatD [prt 4 expr1, doc (showString ">="), prt 5 expr2])
    AbsLanguage.ELeq expr1 expr2 -> prPrec i 4 (concatD [prt 4 expr1, doc (showString "<="), prt 5 expr2])
    AbsLanguage.EGt expr1 expr2 -> prPrec i 4 (concatD [prt 4 expr1, doc (showString ">"), prt 5 expr2])
    AbsLanguage.ELt expr1 expr2 -> prPrec i 4 (concatD [prt 4 expr1, doc (showString "<"), prt 5 expr2])
    AbsLanguage.ELor expr1 expr2 -> prPrec i 5 (concatD [prt 5 expr1, doc (showString "||"), prt 6 expr2])
    AbsLanguage.ELand expr1 expr2 -> prPrec i 5 (concatD [prt 5 expr1, doc (showString "&&"), prt 6 expr2])
    AbsLanguage.EXor expr1 expr2 -> prPrec i 5 (concatD [prt 5 expr1, doc (showString "^^"), prt 6 expr2])
    AbsLanguage.EFnCall id invokeexprlist -> prPrec i 6 (concatD [prt 0 id, prt 0 invokeexprlist])
    AbsLanguage.EIife fundecl invokeexprlist -> prPrec i 6 (concatD [prt 0 fundecl, prt 0 invokeexprlist])
    AbsLanguage.ELValue lvalue -> prPrec i 7 (concatD [prt 0 lvalue])
    AbsLanguage.EString str -> prPrec i 8 (concatD [prt 0 str])
    AbsLanguage.EInt n -> prPrec i 8 (concatD [prt 0 n])
    AbsLanguage.EBool boolean -> prPrec i 8 (concatD [prt 0 boolean])
  prtList _ [x] = concatD [prt 0 x]
  prtList _ (x:xs) = concatD [prt 0 x, doc (showString ","), prt 0 xs]

instance Print [AbsLanguage.Expr] where
  prt = prtList

instance Print AbsLanguage.ExprOrTuple where
  prt i e = case e of
    AbsLanguage.EOTRegular expr -> prPrec i 0 (concatD [prt 0 expr])
    AbsLanguage.EOTTuple exprs -> prPrec i 0 (concatD [doc (showString "["), prt 0 exprs, doc (showString "]")])

instance Print AbsLanguage.Stmt where
  prt i e = case e of
    AbsLanguage.SIf expr stmt -> prPrec i 0 (concatD [doc (showString "if"), doc (showString "("), prt 0 expr, doc (showString ")"), prt 0 stmt])
    AbsLanguage.SIfElse expr stmt1 stmt2 -> prPrec i 0 (concatD [doc (showString "if"), doc (showString "("), prt 0 expr, doc (showString ")"), prt 1 stmt1, doc (showString "else"), prt 0 stmt2])
    AbsLanguage.SFor id expr1 expr2 stmt -> prPrec i 0 (concatD [doc (showString "for"), doc (showString "("), prt 0 id, doc (showString ":"), prt 0 expr1, doc (showString ".."), prt 0 expr2, doc (showString ")"), prt 0 stmt])
    AbsLanguage.SWhile expr stmt -> prPrec i 0 (concatD [doc (showString "while"), doc (showString "("), prt 0 expr, doc (showString ")"), prt 0 stmt])
    AbsLanguage.SExpr expr -> prPrec i 1 (concatD [prt 0 expr, doc (showString ";")])
    AbsLanguage.SVDecl vardecl -> prPrec i 1 (concatD [prt 0 vardecl, doc (showString ";")])
    AbsLanguage.SFDecl id fundecl -> prPrec i 1 (concatD [prt 0 id, doc (showString "::"), prt 0 fundecl])
    AbsLanguage.SSDecl id strcdecl -> prPrec i 1 (concatD [prt 0 id, doc (showString "::"), prt 0 strcdecl])
    AbsLanguage.STDecl tupletarget exprortuple -> prPrec i 1 (concatD [prt 0 tupletarget, doc (showString ":"), doc (showString "="), prt 0 exprortuple, doc (showString ";")])
    AbsLanguage.SAssign lvalue expr -> prPrec i 1 (concatD [prt 0 lvalue, doc (showString "="), prt 0 expr, doc (showString ";")])
    AbsLanguage.STAssign tupletarget exprortuple -> prPrec i 1 (concatD [prt 0 tupletarget, doc (showString "="), prt 0 exprortuple, doc (showString ";")])
    AbsLanguage.SIgnore exprortuple -> prPrec i 1 (concatD [doc (showString "_"), doc (showString "="), prt 0 exprortuple, doc (showString ";")])
    AbsLanguage.SReturn returnexpr -> prPrec i 1 (concatD [doc (showString "return"), prt 0 returnexpr, doc (showString ";")])
    AbsLanguage.SBreak -> prPrec i 1 (concatD [doc (showString "break"), doc (showString ";")])
    AbsLanguage.SCont -> prPrec i 1 (concatD [doc (showString "continue"), doc (showString ";")])
    AbsLanguage.SBlock bind stmts -> prPrec i 1 (concatD [prt 0 bind, doc (showString "{"), prt 0 stmts, doc (showString "}")])
  prtList _ [] = concatD []
  prtList _ (x:xs) = concatD [prt 0 x, prt 0 xs]

instance Print [AbsLanguage.Stmt] where
  prt = prtList

instance Print AbsLanguage.TupleTarget where
  prt i e = case e of
    AbsLanguage.TTar identorignrs -> prPrec i 0 (concatD [doc (showString "["), prt 0 identorignrs, doc (showString "]")])

instance Print AbsLanguage.IdentOrIgnr where
  prt i e = case e of
    AbsLanguage.IOIIdent id -> prPrec i 0 (concatD [prt 0 id])
    AbsLanguage.IOIIgnore -> prPrec i 0 (concatD [doc (showString "_")])
  prtList _ [x] = concatD [prt 0 x]
  prtList _ (x:xs) = concatD [prt 0 x, doc (showString ","), prt 0 xs]

instance Print [AbsLanguage.IdentOrIgnr] where
  prt = prtList

instance Print AbsLanguage.VarDecl where
  prt i e = case e of
    AbsLanguage.DVDecl id type_ -> prPrec i 0 (concatD [prt 0 id, doc (showString ":"), prt 0 type_])
    AbsLanguage.DVDeclAsgn id type_ expr -> prPrec i 0 (concatD [prt 0 id, doc (showString ":"), prt 0 type_, doc (showString "="), prt 0 expr])
    AbsLanguage.DVDeclDeduce id expr -> prPrec i 0 (concatD [prt 0 id, doc (showString ":"), doc (showString "="), prt 0 expr])

instance Print AbsLanguage.StrcDecl where
  prt i e = case e of
    AbsLanguage.SDDefault strcmembers -> prPrec i 0 (concatD [doc (showString "struct"), doc (showString "{"), prt 0 strcmembers, doc (showString "}")])

instance Print AbsLanguage.StrcMembers where
  prt i e = case e of
    AbsLanguage.SMDefault declstrcmembers -> prPrec i 0 (concatD [prt 0 declstrcmembers])
    AbsLanguage.SMEmpty -> prPrec i 0 (concatD [])

instance Print AbsLanguage.DeclStrcMember where
  prt i e = case e of
    AbsLanguage.DStrMem id type_ -> prPrec i 0 (concatD [prt 0 id, doc (showString ":"), prt 0 type_])
  prtList _ [x] = concatD [prt 0 x, doc (showString ";")]
  prtList _ (x:xs) = concatD [prt 0 x, doc (showString ";"), prt 0 xs]

instance Print [AbsLanguage.DeclStrcMember] where
  prt = prtList

instance Print AbsLanguage.FunDecl where
  prt i e = case e of
    AbsLanguage.FDDefault funparams bind funcrett stmts -> prPrec i 0 (concatD [prt 0 funparams, prt 0 bind, prt 0 funcrett, doc (showString "{"), prt 0 stmts, doc (showString "}")])

instance Print AbsLanguage.Bind where
  prt i e = case e of
    AbsLanguage.BdDefault ids -> prPrec i 0 (concatD [doc (showString "!"), doc (showString "("), prt 0 ids, doc (showString ")")])
    AbsLanguage.BdPure -> prPrec i 0 (concatD [doc (showString "!"), doc (showString "("), doc (showString ")")])
    AbsLanguage.BdPureAlt -> prPrec i 0 (concatD [doc (showString "!")])
    AbsLanguage.BdNone -> prPrec i 0 (concatD [])

instance Print AbsLanguage.InvokeExprList where
  prt i e = case e of
    AbsLanguage.IELDefault exprs -> prPrec i 0 (concatD [doc (showString "("), prt 0 exprs, doc (showString ")")])
    AbsLanguage.IELEmpty -> prPrec i 0 (concatD [doc (showString "("), doc (showString ")")])

instance Print AbsLanguage.DeclFunParam where
  prt i e = case e of
    AbsLanguage.DDeclBasic id type_ -> prPrec i 0 (concatD [prt 0 id, doc (showString ":"), prt 0 type_])
  prtList _ [x] = concatD [prt 0 x]
  prtList _ (x:xs) = concatD [prt 0 x, doc (showString ","), prt 0 xs]

instance Print [AbsLanguage.DeclFunParam] where
  prt = prtList

instance Print AbsLanguage.ReturnExpr where
  prt i e = case e of
    AbsLanguage.RExNone -> prPrec i 0 (concatD [])
    AbsLanguage.RExRegular exprortuple -> prPrec i 0 (concatD [prt 0 exprortuple])

instance Print AbsLanguage.FunParams where
  prt i e = case e of
    AbsLanguage.FPList declfunparams -> prPrec i 0 (concatD [doc (showString "("), prt 0 declfunparams, doc (showString ")")])
    AbsLanguage.FPEmpty -> prPrec i 0 (concatD [doc (showString "("), doc (showString ")")])

instance Print AbsLanguage.FuncRetT where
  prt i e = case e of
    AbsLanguage.FRTSingle type_ -> prPrec i 0 (concatD [doc (showString "->"), prt 0 type_])
    AbsLanguage.FRTTuple types -> prPrec i 0 (concatD [doc (showString "->"), doc (showString "["), prt 0 types, doc (showString "]")])
    AbsLanguage.FRTEmpty -> prPrec i 0 (concatD [])

instance Print AbsLanguage.Type where
  prt i e = case e of
    AbsLanguage.TInt -> prPrec i 0 (concatD [doc (showString "int")])
    AbsLanguage.TBool -> prPrec i 0 (concatD [doc (showString "bool")])
    AbsLanguage.TString -> prPrec i 0 (concatD [doc (showString "string")])
    AbsLanguage.TUser id -> prPrec i 0 (concatD [prt 0 id])
  prtList _ [x] = concatD [prt 0 x]
  prtList _ (x:xs) = concatD [prt 0 x, doc (showString ","), prt 0 xs]

instance Print [AbsLanguage.Type] where
  prt = prtList

instance Print [AbsLanguage.Ident] where
  prt = prtList

instance Print AbsLanguage.Boolean where
  prt i e = case e of
    AbsLanguage.BTrue -> prPrec i 0 (concatD [doc (showString "true")])
    AbsLanguage.BFalse -> prPrec i 0 (concatD [doc (showString "false")])

