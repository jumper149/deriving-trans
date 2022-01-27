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

-- | A newtype wrapper for two stacked monad transformers.
--
-- Access instances of the intermediate monad @('t2' 'm')@, whenever 't1' implements
-- 'MonadTrans'/'MonadTransControl'.
--
-- ==== Type level arguments
-- [@'t1' :: ('Type' -> 'Type') -> 'Type' -> 'Type'@] outer monad transformer
-- [@'t2' :: ('Type' -> 'Type') -> 'Type' -> 'Type'@] inner monad transformer
-- [@'m' :: 'Type' -> 'Type'@] monad
-- [@'a' :: 'Type'@] value
type ComposeT :: ((Type -> Type) -> Type -> Type) -- ^ 't1'
              -> ((Type -> Type) -> Type -> Type) -- ^ 't2'
              -> (Type -> Type) -- ^ 'm'
              -> Type -- ^ 'a'
              -> Type
newtype ComposeT t1 t2 m a = ComposeT { deComposeT :: t1 (t2 m) a }
  deriving newtype (Applicative, Functor, Monad)

instance (forall m. Monad m => Monad (t2 m), MonadTrans t1, MonadTrans t2) => MonadTrans (ComposeT t1 t2) where
  lift = ComposeT . lift . lift

instance (forall m. Monad m => Monad (t2 m), MonadTransControl t1, MonadTransControl t2) => MonadTransControl (ComposeT t1 t2) where
  type StT (ComposeT t1 t2) a = StT t2 (StT t1 a)
  liftWith f = defaultLiftWith2 ComposeT deComposeT $ \ x -> f x
  restoreT = defaultRestoreT2 ComposeT

-- | Elevated to 'm'.
deriving via Elevator (ComposeT t1 t2) m
  instance
    ( Monad (t1 (t2 m))
    , MonadTrans (ComposeT t1 t2)
    , MonadIO m
    ) => MonadIO (ComposeT t1 t2 m)

-- | Elevated to 'm'.
deriving via Elevator (ComposeT t1 t2) m
  instance
    ( Monad (t1 (t2 m))
    , MonadTrans (ComposeT t1 t2)
    , MonadBase b m
    ) => MonadBase b (ComposeT t1 t2 m)

-- | Elevated to 'm'.
deriving via Elevator (ComposeT t1 t2) m
  instance
    ( Monad (t1 (t2 m))
    , MonadTransControl (ComposeT t1 t2)
    , MonadBaseControl b m
    ) => MonadBaseControl b (ComposeT t1 t2 m)

-- | Elevated to @('t2' 'm')@.
deriving via Elevator t1 (t2 (m :: * -> *))
  instance
    ( Monad (t1 (t2 m))
    , MonadTransControl t1
    , MonadError e (t2 m)
    ) => MonadError e (ComposeT t1 t2 m)

-- | Elevated to @('t2' 'm')@.
deriving via Elevator t1 (t2 (m :: * -> *))
  instance
    ( Monad (t1 (t2 m))
    , MonadTransControl t1
    , MonadReader r (t2 m)
    ) => MonadReader r (ComposeT t1 t2 m)

-- | Elevated to @('t2' 'm')@.
deriving via Elevator t1 (t2 (m :: * -> *))
  instance
    ( Monad (t1 (t2 m))
    , MonadTrans t1
    , MonadState s (t2 m)
    ) => MonadState s (ComposeT t1 t2 m)

-- | Elevated to @('t2' 'm')@.
deriving via Elevator t1 (t2 (m :: * -> *))
  instance
    ( Monad (t1 (t2 m))
    , MonadTransControl t1
    , MonadWriter w (t2 m)
    ) => MonadWriter w (ComposeT t1 t2 m)

-- | Run a transformer stack.
--
-- This function takes the two individual monad transformer runners as arguments.
runComposeT :: (forall a. t1 (t2 m) a -> t2 m (StT t1 a)) -- ^ run 't1'
            -> (forall a. t2 m a -> m (StT t2 a)) -- ^ run 't2'
            -> (forall a. ComposeT t1 t2 m a -> m (StT t2 (StT t1 a)))
runComposeT runT1 runT2 = runT2 . runT1 . deComposeT

-- | Equivalent to 'runComposeT', but discards the monadic state 'StT'.
-- This is a simple approach when your monad transformer stack doesn't carry monadic state.
--
-- @
-- 'StT' ('ComposeT' 't1' 't2') a ~ a
-- @
--
-- This can be used to improve error messages when modifying a monad transformer stack.
runComposeT' :: (t1 (t2 m) a -> t2 m a) -- ^ run 't1'
             -> (t2 m a -> m a) -- ^ run 't2'
             -> (ComposeT t1 t2 m a -> m a)
runComposeT' runT1 runT2 = runT2 . runT1 . deComposeT
