module Errors where

import Control.Applicative

data ExceptT e m a = ExceptT {
    runExceptT :: m (Either e a)
}

instance Functor m => Functor (ExceptT e m) where
  fmap f = ExceptT . fmap (fmap f) . runExceptT

instance Applicative m => Applicative (ExceptT e m) where
  pure    = ExceptT . pure . Right
  f <*> x = ExceptT $ liftA2 (<*>) (runExceptT f) (runExceptT x)

instance Monad m => Monad (ExceptT e m) where
  return  = ExceptT . return . Right
  x >>= f = ExceptT $ runExceptT x >>= either (return . Left) (runExceptT . f)

liftEither :: Monad m => Either e a -> ExceptT e m a
liftEither x = ExceptT (return x)

lift :: Functor m => m a -> ExceptT e m a
lift x = ExceptT (fmap Right x)

throwE :: Monad m => e -> ExceptT e m a
throwE x = liftEither (Left x)

catchE :: Monad m => ExceptT e m a -> (e -> ExceptT c m a) -> ExceptT c m a
catchE throwing handler = ExceptT $ do
  x <- runExceptT throwing
  case x of
    Left failure  -> runExceptT (handler failure)
    Right success -> return (Right success)


---

data Error = NoSuchUser | NullId
