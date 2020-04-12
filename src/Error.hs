-- | Generalized Error Monad (including Monad Transform) to combine error
--   handling with with IO. This features an ErrorDetail type which allows us to
--   give user many detailed information about an error. This is laregly taken
--   from MaybeT implementation in the Haskell standard library.
module Error where

import Control.Monad.Trans.Class (lift, MonadTrans(..))

type PPos_ = Maybe (Int, Int) -- TODO: Kill repeating definitions of PPos

data ErrorDetail
  = EDVarNotFound String PPos_
  | EDParsingError String
  | EDTypeError String String PPos_
  | EDTypeNotFound String PPos_
  | NotImplemented String -- TODO? This should not happen in the final version

-- TODO: hardcoded!!!
file_ :: String
file_ = "tests.txt"

showFCol :: PPos_ -> String
showFCol (Just (l, c)) = file_ ++ ":" ++ show l ++ ":" ++ show c ++ ": "
showFCol Nothing = file_ ++ ": "

-- TODO: unify showfcol to be called somewhere else. and do getpos for error detail.
instance Show ErrorDetail where
  show (EDVarNotFound name p) = showFCol p ++ "Variable `" ++ name ++ "' not in scope."
  show (EDParsingError str) = "Parsing error: " ++ str ++ "."
  show (EDTypeError expected got p) = (showFCol p ++ "Type error: "
                                       ++ "expected `" ++ expected ++ "'"
                                       ++ ", got `" ++ got ++ "'.")
  show (EDTypeNotFound tname p) = showFCol p ++ "Type `" ++ tname ++ "' not in scope."
  show _ = "Unknown error: No idea what is happening." -- TODO.

data Error a
  = Ok a
  | Fail ErrorDetail
  deriving (Show)

instance Functor Error  where
  fmap _ (Fail reason) = Fail reason
  fmap f (Ok a) = Ok (f a)

instance Applicative Error where
  pure = Ok

  Ok f <*> m = fmap f m
  Fail reason <*> _m = Fail reason

  Ok _m1 *> m2 = m2
  Fail reason *> _m2 = Fail reason -- TODO(MD): Invesitgate

instance Monad Error where
  (Ok x) >>= k = k x
  Fail reason >>= _ = Fail reason

  (>>) = (*>) -- TODO(MD): Invesitgate

newtype ErrorT m a = ErrorT { runErrorT :: m (Error a) }

instance MonadTrans ErrorT where
  lift = ErrorT . fmap Ok

instance (Monad m) => Monad (ErrorT m) where
  return = ErrorT . return . Ok

  x >>= f = ErrorT $ do
      v <- runErrorT x
      case v of
          Fail reason -> return $ Fail reason
          Ok w -> runErrorT (f w)

instance (Functor m) => Functor (ErrorT m) where
  fmap f = ErrorT . fmap (fmap f) . runErrorT

instance (Functor m, Monad m) => Applicative (ErrorT m) where
  pure = ErrorT . return . Ok

  mf <*> mx = ErrorT $ do
      mb_f <- runErrorT mf
      case mb_f of
          Fail reason -> return $ Fail reason
          Ok f -> do
              mb_x <- runErrorT mx
              case mb_x of
                  Fail reason -> return $ Fail reason
                  Ok x  -> return (Ok (f x))

  m *> k = m >> k -- TODO(MD): Invesitgate

-- | Convert Maybe a to Error a. If value is Nothing return an error with
--   provided description.
errorFromMaybe :: ErrorDetail -> Maybe a -> Error a
errorFromMaybe _ (Just x) = Ok x
errorFromMaybe det Nothing = Fail det

-- | Used to promote regular Error into ErrorT with any wrapped monad.
toErrorT :: Monad m => Error a -> ErrorT m a
toErrorT = ErrorT . return
