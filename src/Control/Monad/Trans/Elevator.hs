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

newtype Elevator
  (t :: (Type -> Type) -> Type -> Type)
  (m :: Type -> Type)
  (a :: Type)
    = Ascend { descend :: t m a }
  deriving newtype (Applicative, Functor, Monad)

instance (Monad (t m), MonadTrans t, MonadBase b m) => MonadBase b (Elevator t m) where
  liftBase = Ascend . lift . liftBase

instance (Monad (t m), MonadTransControl t, MonadBaseControl b m) => MonadBaseControl b (Elevator t m) where
  type StM (Elevator t m) a = StM m (StT t a)
  liftBaseWith f = Ascend $ liftWith $ \ runT -> liftBaseWith $ \ runInBase -> f $ runInBase . runT . descend
  restoreM = Ascend . restoreT . restoreM

instance (Monad (t m), MonadTrans t, MonadIO m) => MonadIO (Elevator t m) where
  liftIO = Ascend . lift . liftIO

instance (Monad (t m), MonadTransControl t, MonadError e m) => MonadError e (Elevator t m) where
  throwError = Ascend . lift . throwError
  catchError throwing catching = Ascend $ (restoreT . pure =<<) $ liftWith $ \ runT ->
    catchError (runT $ descend throwing) (runT . descend . catching)

instance (Monad (t m), MonadTransControl t, MonadReader r m) => MonadReader r (Elevator t m) where
  ask = Ascend $ lift ask
  local f tma = Ascend $ (restoreT . pure =<<) $ liftWith $ \ runT ->
    local f $ runT $ descend tma

instance (Monad (t m), MonadTrans t, MonadState s m) => MonadState s (Elevator t m) where
  get = Ascend $ lift get
  put = Ascend . lift . put

instance (Monad (t m), MonadTransControl t, MonadWriter w m) => MonadWriter w (Elevator t m) where
  tell = Ascend . lift . tell
  listen tma = Ascend $ liftWith (\ runT -> listen $ runT $ descend tma) >>= \ (sta, w) ->
    (, w) <$> restoreT (pure sta)
  pass tma = Ascend $ lift . pass . pure =<< descend tma
