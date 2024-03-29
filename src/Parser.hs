{-# LANGUAGE FlexibleInstances #-}

-- This files combines all bnfc + Alex + Happy generated files and provides
-- minimal interface so that machine generated files don't have to be included
-- in the main program.
module Parser where

import AbsLanguage
import ParLanguage (pProgram, myLexer)

import qualified ErrM

import System.IO (hPutStrLn, stderr)

import Common
import Error

-- Do lexing, then parsing. Lexing can't fail and by combing these functions we
-- don't have to include LexLanguage in this file. Convert ErrM to our Error.
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
  getPos (SAssert pos _) = pos
  getPos (SPrint pos _) = pos
  getPos (SCont pos) = pos
  getPos (SBlock pos _ _) = pos

instance Pos (Expr (Maybe (Int, Int))) where
  getPos (EPlus pos _ _) = pos
  getPos (EMinus pos _ _) = pos
  getPos (ECat pos _ _) = pos
  getPos (ETimes pos _ _) = pos
  getPos (EDiv pos _ _) = pos
  getPos (EMod pos _ _) = pos
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
  getPos (EScan pos _) = pos
  getPos (EIife pos _ _) = pos
  getPos (ELValue pos _) = pos
  getPos (ENew pos _ _) = pos
  getPos (EString pos _) = pos
  getPos (EInt pos _) = pos
  getPos (EBool pos _) = pos

instance Pos (LValue (Maybe (Int, Int))) where
  getPos (LValueVar pos _) = pos
  getPos (LValueMemb pos _ _) = pos

printErr :: String -> IO ()
printErr = hPutStrLn stderr
