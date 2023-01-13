{-# OPTIONS_GHC -fno-warn-orphans #-}

module Control.Monad.Accum.OrphanInstances where

import Control.Monad.Accum
import qualified Control.Monad.Trans.Accum as T

instance (Monoid w, Monad m) => MonadAccum w (T.AccumT w m) where
  look = T.look
  add = T.add
