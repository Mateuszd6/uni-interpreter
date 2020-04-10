-- | This files combines all bnfc + Alex + Happy generated files and provides
-- minimal interface so that machine generated files don't have to be included
-- in the main program.
module Parser (parseProgram, PPos, Token) where

import Error

-- Auto-generated files:
import ParLanguage (pProgram, myLexer)
import LexLanguage (Token)
import AbsLanguage
import ErrM -- Error monad used by BNFC and ParLanguage. We keep it's use to its
            -- file so that it does not appear in the main code.

type PPos = Maybe (Int, Int)

-- Do lexing, then parsing. Lexing can't fail and by combing these functions we
-- don't have to include LexLanguage in this file.
parseProgram :: String -> Error (Program PPos)
parseProgram = convertToError . pProgram . myLexer
  where
    convertToError :: Err a -> Error a
    convertToError (Ok x) = Ok_ x
    convertToError (Bad reason) = Fail_ $ ParsingError reason
