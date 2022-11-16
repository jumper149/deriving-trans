module Countdown.Mtl where

import qualified Control.Monad.Reader as M
import qualified Control.Monad.State as M
import Data.Functor.Identity

programMtl :: M.MonadState Integer m => m Integer
programMtl = do
  n <- M.get @Integer
  if n <= 0
    then pure n
    else do
      M.put (n - 1)
      programMtl
{-# NOINLINE programMtl #-}

countdownMtl :: Integer -> (Integer, Integer)
countdownMtl n = flip M.runState n $ programMtl

countdownMtlDeep :: Integer -> (Integer, Integer)
countdownMtlDeep n = runIdentity
  . runR . runR . runR . runR . runR
  . flip M.runStateT n
  . runR . runR . runR . runR . runR
  $ programMtl
  where
    runR = flip M.runReaderT ()
