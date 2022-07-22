{-# LANGUAGE UndecidableInstances #-}

module Control.Monad.Trans.Compose.Transparent (
  TransparentT,
  runTransparentT,
) where

import Control.Monad.Trans.Class
import Control.Monad.Trans.Control
import Control.Monad.Trans.Control.Identity
import Control.Monad.Trans.Elevator
import Control.Monad.Trans.Identity

-- | A monad transformer, that passes through all instances via 'Elevator'.
type TransparentT = Elevator NoT

runTransparentT :: TransparentT m a -> m a
runTransparentT = runIdentityT . runNoT . descend

-- | A newtype wrapper around 'IdentityT'.
newtype NoT m a = MkNoT {runNoT :: IdentityT m a}
  deriving newtype (Functor, Applicative, Monad)
  deriving newtype (MonadTrans, MonadTransControl, MonadTransControlIdentity)
