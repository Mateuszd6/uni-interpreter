-- | TODO: Describe and disclaim that it is MaybeT copied from haskell standard
--   libarry

module Error where

import Control.Monad (liftM)
import Control.Monad.Trans.Class (lift, MonadTrans(..))

data ErrorDetail
  = ParsingError String
  | TypeError -- TOOD: refactor
  | NotImplemented String -- TODO? This should not happen in the final version
  deriving (Show)

data Error a
  = Ok_ a
  | Fail_ ErrorDetail
  deriving (Show)

instance Functor Error  where
  fmap _ (Fail_ reason) = Fail_ reason
  fmap f (Ok_ a) = Ok_ (f a)

instance Applicative Error where
  pure = Ok_

  Ok_ f <*> m = fmap f m
  Fail_ reason <*> _m = Fail_ reason

  Ok_ _m1 *> m2 = m2
  Fail_ reason *> _m2 = Fail_ reason -- TODO(MD): Invesitgate

instance Monad Error where
  (Ok_ x) >>= k = k x
  Fail_ reason >>= _ = Fail_ reason

  (>>) = (*>) -- TODO(MD): Invesitgate

newtype ErrorT m a = ErrorT { runErrorT :: m (Error a) }

instance MonadTrans ErrorT where
  lift = ErrorT . liftM Ok_

instance (Monad m) => Monad (ErrorT m) where
  return = ErrorT . return . Ok_

  x >>= f = ErrorT $ do
      v <- runErrorT x
      case v of
          Fail_ reason -> return $ Fail_ reason
          Ok_ w -> runErrorT (f w)

instance (Functor m) => Functor (ErrorT m) where
  fmap f = ErrorT . fmap (fmap f) . runErrorT

instance (Functor m, Monad m) => Applicative (ErrorT m) where
  pure = ErrorT . return . Ok_

  mf <*> mx = ErrorT $ do
      mb_f <- runErrorT mf
      case mb_f of
          Fail_ reason -> return $ Fail_ reason
          Ok_ f -> do
              mb_x <- runErrorT mx
              case mb_x of
                  Fail_ reason -> return $ Fail_ reason
                  Ok_ x  -> return (Ok_ (f x))

  m *> k = m >>= \_ -> k -- TODO(MD): Invesitgate
