-- BNF Converter: Error Monad
-- Copyright (C) 2004  Author:  Aarne Ranta

-- This file comes with NO WARRANTY and may be used FOR ANY PURPOSE.
module ErrM where

-- the Error monad: like Maybe type with error msgs

import Control.Monad (MonadPlus(..), liftM)
import Control.Applicative (Alternative(..))

data ErrorDescr
  = TypeError
  | RuntimeError
  | FuckHaskellError -- TODO: Just make sure to remove it.
  deriving (Read, Show, Eq, Ord)

-- Bad is kept for compatibility with Happy parser
data Err a = Ok a | Bad String | Error ErrorDescr --  | Error String -- TODO: It turns out it can be adjusted
  deriving (Read, Show, Eq, Ord)

instance Monad Err where
  return      = Ok
  -- fail        = Bad -- TODO: Revive?
  Ok a  >>= f = f a
  Bad s >>= _ = Bad s
  Error x >>= _ = Error x
  -- Error s >>= _ = Error s

instance Applicative Err where
  pure = Ok
  (Ok f) <*> o  = liftM f o
  (Bad s) <*> _ = Bad s
  (Error s) <*> _ = Error s

instance Functor Err where
  fmap = liftM

instance MonadPlus Err where
  mzero = Bad "Err.mzero"
  mplus (Bad _) y = y
  mplus x       _ = x

instance Alternative Err where
  empty = mzero
  (<|>) = mplus
