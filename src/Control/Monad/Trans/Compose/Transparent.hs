{-# LANGUAGE UndecidableInstances #-}

module Control.Monad.Trans.Compose.Transparent where

import Control.Monad.Trans.Class
import Control.Monad.Trans.Control
import Control.Monad.Trans.Control.Identity
import Control.Monad.Trans.Elevator
import Control.Monad.Trans.Identity
import Data.Kind

-- | A monad transformer, that passes through all instances via 'Elevator'.
--
-- This cannot be defined as a newtype, because we want all the instances, that are defined for
-- 'Elevator' to work for 'TransparentT'.
type TransparentT = Elevator NoT

runTransparentT :: TransparentT m a -> m a
runTransparentT = runIdentityT . unNoT . descend

-- | A newtype wrapper around 'IdentityT'.
--
-- This is used in 'TransparentT' to encourage the use of 'runTransparentT'.
type NoT :: (Type -> Type) -> Type -> Type
type role NoT representational nominal
newtype NoT m a = MkNoT {unNoT :: IdentityT m a}
  deriving newtype (Functor, Applicative, Monad)
  deriving newtype (MonadTrans, MonadTransControl, MonadTransControlIdentity)

{-# WARNING NoT, MkNoT, unNoT "This is an implementation detail of 'TransparentT'." #-}
