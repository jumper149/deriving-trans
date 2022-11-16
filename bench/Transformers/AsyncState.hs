{-# LANGUAGE UndecidableInstances #-}

module Transformers.AsyncState where

import Transformers.AsyncState.Class

import Control.Monad.Trans
import Control.Monad.Trans.Compose
import Control.Monad.Trans.Control
import Control.Monad.Trans.Control.Identity
import Control.Monad.Trans.Reader
import Data.Kind
import GHC.Conc

type ReaderTVarT :: Type -> (Type -> Type) -> Type -> Type
newtype ReaderTVarT s m a = MkReaderTVarT {unReaderTVarT :: ReaderT (TVar s) m a}
  deriving newtype (Applicative, Functor, Monad)
  deriving newtype (MonadTrans, MonadTransControl, MonadTransControlIdentity)
  deriving newtype (MonadIO)

instance MonadIO m => MonadReaderTVar s (ReaderTVarT s m) where
  askTVar = MkReaderTVarT ask

deriving via
  ReaderTVarT s ((t2 :: (Type -> Type) -> Type -> Type) m)
  instance
    MonadIO (t2 m) => MonadReaderTVar s (ComposeT (ReaderTVarT s) t2 m)

runReaderTVarT :: ReaderTVarT s m a -> TVar s -> m a
runReaderTVarT = runReaderT . unReaderTVarT
{-# INLINE runReaderTVarT #-}
