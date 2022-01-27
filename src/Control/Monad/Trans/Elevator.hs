{-# LANGUAGE QuantifiedConstraints, UndecidableInstances, TupleSections #-}

module Control.Monad.Trans.Elevator where

import Control.Monad.Base
import Control.Monad.Error.Class
import Control.Monad.Reader.Class
import Control.Monad.State.Class
import Control.Monad.Trans
import Control.Monad.Trans.Control
import Control.Monad.Writer.Class
import Data.Kind

-- | A newtype wrapper for monad transformers.
--
-- Access instances of the inner monad @m@.
newtype Elevator
  (t :: (Type -> Type) -> Type -> Type)
  (m :: Type -> Type)
  (a :: Type)
    = Ascend { descend :: t m a }
  deriving newtype (Applicative, Functor, Monad)
  deriving newtype (MonadTrans, MonadTransControl)

instance (Monad (t m), MonadTrans t, MonadBase b m) => MonadBase b (Elevator t m) where
  liftBase = lift . liftBase

instance (Monad (t m), MonadTransControl t, MonadBaseControl b m) => MonadBaseControl b (Elevator t m) where
  type StM (Elevator t m) a = StM m (StT t a)
  liftBaseWith f = liftWith $ \ runT -> liftBaseWith $ \ runInBase -> f $ runInBase . runT
  restoreM = restoreT . restoreM

instance (Monad (t m), MonadTrans t, MonadIO m) => MonadIO (Elevator t m) where
  liftIO = lift . liftIO

instance (Monad (t m), MonadTransControl t, MonadError e m) => MonadError e (Elevator t m) where
  throwError = lift . throwError
  catchError throwing catching = (restoreT . pure =<<) $ liftWith $ \ runT ->
    catchError (runT throwing) (runT . catching)

instance (Monad (t m), MonadTransControl t, MonadReader r m) => MonadReader r (Elevator t m) where
  ask = lift ask
  local f tma = (restoreT . pure =<<) $ liftWith $ \ runT ->
    local f $ runT tma

instance (Monad (t m), MonadTrans t, MonadState s m) => MonadState s (Elevator t m) where
  get = lift get
  put = lift . put

instance (Monad (t m), MonadTransControl t, MonadWriter w m) => MonadWriter w (Elevator t m) where
  tell = lift . tell
  listen tma = liftWith (\ runT -> listen $ runT tma) >>= \ (sta, w) ->
    (, w) <$> restoreT (pure sta)
  pass tma = lift . pass . pure =<< tma
