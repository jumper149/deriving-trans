{-# OPTIONS_GHC -fno-warn-orphans #-}

module Control.Monad.Select.OrphanInstances where

import Control.Monad.Select
import Control.Monad.Turn
import qualified Control.Monad.Trans.Select as T

instance MonadTurn m => MonadSelect r (T.SelectT r m) where
  select f = T.SelectT $ \ k -> returnWith $ \turn -> f $ turn . k
