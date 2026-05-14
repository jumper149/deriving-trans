{-# LANGUAGE UndecidableInstances #-}

module Transformers.AsyncState.Class where

import Control.Monad.Trans
import Control.Monad.Trans.Compose
import Control.Monad.Trans.Control.Identity
import Control.Monad.Trans.Elevator
import Control.Monad.Trans.Reader
import Data.Functor.Compose
import Data.Kind
import Data.Proxy
import GHC.Conc

type MonadReaderTVar :: Type -> (Type -> Type) -> Constraint
class Monad m => MonadReaderTVar s m | m -> s where
  askTVar :: m (TVar s)

instance
  ( Monad (t m)
  , MonadTrans t
  , MonadReaderTVar s m
  ) =>
  MonadReaderTVar s (Elevator t m)
  where
  askTVar = lift askTVar

deriving via
  Elevator t1 ((t2 :: (Type -> Type) -> Type -> Type) m)
  instance
  {-# OVERLAPPABLE #-}
    ( Monad (t1 (t2 m))
    , MonadTrans t1
    , MonadReaderTVar s (t2 m)
    ) =>
    MonadReaderTVar s (ComposeT t1 t2 m)
deriving via
  Elevator (ReaderT r) m
  instance MonadReaderTVar s m
    =>
    MonadReaderTVar s (ReaderT r m)
