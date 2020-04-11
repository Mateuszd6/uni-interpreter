-- | Generalized Error Monad (including Monad Transform) to combine error
--   handling with with IO. This features an ErrorDetail type which allows us to
--   give user many detailed information about an error. This is laregly taken
--   from MaybeT implementation in the Haskell standard library.
module Error where

import Control.Monad.Trans.Class (lift, MonadTrans(..))

data ErrorDetail
  = ParsingError String
  | TypeError -- TOOD: refactor
  | NotImplemented String -- TODO? This should not happen in the final version
  deriving (Show)

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

-- | Used to promote regular Error into ErrorT with any wrapped monad.
toErrorT :: Monad m => Error a -> ErrorT m a
toErrorT = ErrorT . return
