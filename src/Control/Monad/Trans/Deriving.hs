{-# LANGUAGE QuantifiedConstraints, UndecidableInstances #-}

module Control.Monad.Trans.Deriving (
  Stack0T (..)
, Stack1T (..)
, Stack2T (..)
, Stack3T (..)
) where

import Control.Monad.Base
import Control.Monad.Trans
import Control.Monad.Trans.Control
import Data.Kind

newtype Stack0T
  (m :: Type -> Type)
  (a :: Type)
    = Stack0T { unStack0T :: m a }
  deriving (Applicative, Functor, Monad, MonadBase b, MonadBaseControl b)

instance MonadTrans Stack0T where
  lift = Stack0T

instance MonadTransControl Stack0T where
  type StT Stack0T a = a
  liftWith f = Stack0T $ f unStack0T
  restoreT = Stack0T

newtype Stack1T
  (t :: (Type -> Type) -> Type -> Type)
  (m :: Type -> Type)
  (a :: Type)
    = Stack1T { unStack1T :: t m a }
  deriving (Applicative, Functor, Monad, MonadBase b, MonadBaseControl b, MonadTrans, MonadTransControl)

newtype Stack2T
  (t1 :: (Type -> Type) -> Type -> Type)
  (t2 :: (Type -> Type) -> Type -> Type)
  (m :: Type -> Type)
  (a :: Type)
    = Stack2T { unStack2T :: t1 (t2 m) a }
  deriving (Applicative, Functor, Monad, MonadBase b, MonadBaseControl b)

instance (forall m. Monad m => Monad (t2 m), MonadTrans t1, MonadTrans t2) => MonadTrans (Stack2T t1 t2) where
  lift = Stack2T . lift . lift

instance (forall m. Monad m => Monad (t2 m), MonadTransControl t1, MonadTransControl t2) => MonadTransControl (Stack2T t1 t2) where
  type StT (Stack2T t1 t2) a = StT t2 (StT t1 a)
  liftWith f = defaultLiftWith2 Stack2T unStack2T $ \x -> f x
  restoreT = defaultRestoreT2 Stack2T

newtype Stack3T
  (t1 :: (Type -> Type) -> Type -> Type)
  (t2 :: (Type -> Type) -> Type -> Type)
  (t3 :: (Type -> Type) -> Type -> Type)
  (m :: Type -> Type)
  (a :: Type)
    = Stack3T { unStack3T :: t1 (t2 (t3 m)) a }
  deriving (Applicative, Functor, Monad, MonadBase b, MonadBaseControl b)

instance (forall m. Monad m => Monad (t3 m), forall m. Monad m => Monad (t2 (t3 m)), MonadTrans t1, MonadTrans t2, MonadTrans t3) => MonadTrans (Stack3T t1 t2 t3) where
  lift = Stack3T . lift . lift . lift

instance (forall m. Monad m => Monad (t3 m), forall m. Monad m => Monad (t2 (t3 m)), MonadTransControl t1, MonadTransControl t2, MonadTransControl t3) => MonadTransControl (Stack3T t1 t2 t3) where
  type StT (Stack3T t1 t2 t3) a = StT t3 (StT t2 (StT t1 a))
  liftWith f = Stack3T $ liftWith $ \run -> liftWith $ \run' -> liftWith $ \ run'' -> f $ run'' . run' . run . unStack3T
  restoreT = Stack3T . restoreT . restoreT . restoreT
