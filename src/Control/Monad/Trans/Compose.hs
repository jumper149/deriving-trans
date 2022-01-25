{-# LANGUAGE QuantifiedConstraints, UndecidableInstances #-}

module Control.Monad.Trans.Compose where

import Control.Monad.Base
import Control.Monad.Error.Class
import Control.Monad.IO.Class
import Control.Monad.Reader.Class
import Control.Monad.State.Class
import Control.Monad.Trans
import Control.Monad.Trans.Control
import Control.Monad.Trans.Elevator
import Control.Monad.Writer.Class
import Data.Kind

newtype ComposeT
  (t1 :: (Type -> Type) -> Type -> Type)
  (t2 :: (Type -> Type) -> Type -> Type)
  (m :: Type -> Type)
  (a :: Type)
    = ComposeT { deComposeT :: t1 (t2 m) a }
  deriving newtype (Applicative, Functor, Monad)

instance (forall m. Monad m => Monad (t2 m), MonadTrans t1, MonadTrans t2) => MonadTrans (ComposeT t1 t2) where
  lift = ComposeT . lift . lift

instance (forall m. Monad m => Monad (t2 m), MonadTransControl t1, MonadTransControl t2) => MonadTransControl (ComposeT t1 t2) where
  type StT (ComposeT t1 t2) a = StT t2 (StT t1 a)
  liftWith f = defaultLiftWith2 ComposeT deComposeT $ \ x -> f x
  restoreT = defaultRestoreT2 ComposeT

-- | Elevated to `m`.
deriving via Elevator (ComposeT t1 t2) m
  instance
    ( Monad (t1 (t2 m))
    , MonadTrans (ComposeT t1 t2)
    , MonadIO m
    ) => MonadIO (ComposeT t1 t2 m)

-- | Elevated to `m`.
deriving via Elevator (ComposeT t1 t2) m
  instance
    ( Monad (t1 (t2 m))
    , MonadTrans (ComposeT t1 t2)
    , MonadBase b m
    ) => MonadBase b (ComposeT t1 t2 m)

-- | Elevated to `m`.
deriving via Elevator (ComposeT t1 t2) m
  instance
    ( Monad (t1 (t2 m))
    , MonadTransControl (ComposeT t1 t2)
    , MonadBaseControl b m
    ) => MonadBaseControl b (ComposeT t1 t2 m)

-- | Elevated to `t2 m`.
deriving via Elevator t1 (t2 (m :: * -> *))
  instance
    ( Monad (t1 (t2 m))
    , MonadTransControl t1
    , MonadError e (t2 m)
    ) => MonadError e (ComposeT t1 t2 m)

-- | Elevated to `t2 m`.
deriving via Elevator t1 (t2 (m :: * -> *))
  instance
    ( Monad (t1 (t2 m))
    , MonadTransControl t1
    , MonadReader r (t2 m)
    ) => MonadReader r (ComposeT t1 t2 m)

-- | Elevated to `t2 m`.
deriving via Elevator t1 (t2 (m :: * -> *))
  instance
    ( Monad (t1 (t2 m))
    , MonadTrans t1
    , MonadState s (t2 m)
    ) => MonadState s (ComposeT t1 t2 m)

-- | Elevated to `t2 m`.
deriving via Elevator t1 (t2 (m :: * -> *))
  instance
    ( Monad (t1 (t2 m))
    , MonadTransControl t1
    , MonadWriter w (t2 m)
    ) => MonadWriter w (ComposeT t1 t2 m)

runComposeT :: (forall a. t1 (t2 m) a -> t2 m (StT t1 a))
            -> (forall a. t2 m a -> m (StT t2 a))
            -> (forall a. ComposeT t1 t2 m a -> m (StT t2 (StT t1 a)))
runComposeT runT1 runT2 = runT2 . runT1 . deComposeT

runComposeT' :: (t1 (t2 m) a -> t2 m a)
             -> (t2 m a -> m a)
             -> (ComposeT t1 t2 m a -> m a)
runComposeT' runT1 runT2 = runT2 . runT1 . deComposeT
