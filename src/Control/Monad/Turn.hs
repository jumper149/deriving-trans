{-# LANGUAGE UndecidableInstances #-}

module Control.Monad.Turn where

import Control.Monad.Trans.Control.Identity
import Data.Functor.Identity

class Monad m => BaseMonadTurn m where
  returnBaseWith :: ((forall x. m x -> x) -> a) -> m a

instance BaseMonadTurn Identity where
  returnBaseWith f = Identity $ f runIdentity

instance BaseMonadTurn ((->) r) where
  returnBaseWith f r = f ($ r)

class Monad m => MonadTurn m where
  returnWith :: ((forall x. m x -> x) -> a) -> m a

-- TODO: Remove `Monad m` constraint. Found a GHC bug. :/
instance (BaseMonadTurn b, MonadBaseControlIdentity b m, Monad m) => MonadTurn m where
  returnWith f =
    liftBaseWithIdentity $ \ runInBase ->
      returnBaseWith $ \ turn ->
        f $ turn . runInBase

instance (MonadTurn m, MonadTransControlIdentity t) => MonadTurn (t m) where
  returnWith f =
    liftWithIdentity $ \ runT ->
      returnWith $ \ turn ->
        f $ turn . runT
