{-# LANGUAGE FlexibleInstances #-}

-- | This files combines all bnfc + Alex + Happy generated files and provides
-- minimal interface so that machine generated files don't have to be included
-- in the main program.
module Parser where


-- Auto-generated files:
import ParLanguage (pProgram, myLexer)
import AbsLanguage
import qualified ErrM

import State

-- TODO: describe that it is a parsing position.
-- TODO: Note that the program requires speciyfic version of BNFC
-- type PPos = Maybe (Int, Int)

-- Do lexing, then parsing. Lexing can't fail and by combing these functions we
-- don't have to include LexLanguage in this file. Instead of using ErrM
-- generated by bnfc convert result to our Error class, which is used to store
-- much detailed error desription for any kind of error across the program.
parseProgram :: String -> Error (Program PPos)
parseProgram = convertToError . pProgram . myLexer
  where
    convertToError :: ErrM.Err a -> Error a
    convertToError (ErrM.Ok x) = Ok x
    convertToError (ErrM.Bad reason) = Fail $ EDParsingError reason

-- This is the iface for getting a position info from the syntax element.
class Pos a where
  getPos :: a -> Maybe (Int, Int)

instance Pos (Stmt (Maybe (Int, Int))) where
  getPos (SIf pos _ _) = pos
  getPos (SIfElse pos _ _ _) = pos
  getPos (SFor pos _ _ _ _) = pos
  getPos (SWhile pos _ _) = pos
  getPos (SExpr pos _) = pos
  getPos (SVDecl pos _) = pos
  getPos (SFDecl pos _ _) = pos
  getPos (SSDecl pos _ _) = pos
  getPos (STDecl pos _ _) = pos
  getPos (SAssign pos _  _) = pos
  getPos (STAssign pos _ _) = pos
  getPos (SIgnore pos _) = pos
  getPos (SReturn pos _) = pos
  getPos (SBreak pos) = pos
  getPos (SCont pos) = pos
  getPos (SBlock pos _ _) = pos

instance Pos (Expr (Maybe (Int, Int))) where
  getPos (EPlus pos _ _) = pos
  getPos (EMinus pos _ _) = pos
  getPos (ECat pos _ _) = pos
  getPos (ETimes pos _ _) = pos
  getPos (EDiv pos _ _) = pos
  getPos (EPow pos _ _) = pos
  getPos (EEq pos _ _) = pos
  getPos (ENeq pos _ _) = pos
  getPos (EGeq pos _ _) = pos
  getPos (ELeq pos _ _) = pos
  getPos (EGt pos _ _) = pos
  getPos (ELt pos _ _) = pos
  getPos (ELor pos _ _) = pos
  getPos (ELand pos _ _) = pos
  getPos (EXor pos _ _) = pos
  getPos (EFnCall pos _ _) = pos
  getPos (EIife pos _ _) = pos
  getPos (ELValue pos _) = pos
  getPos (EString pos _) = pos
  getPos (EInt pos _) = pos
  getPos (EBool pos _) = pos
