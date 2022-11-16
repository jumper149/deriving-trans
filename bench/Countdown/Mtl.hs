module Countdown.Mtl where

import qualified Control.Monad.Reader as M
import qualified Control.Monad.State as M
import Data.Functor.Identity

import qualified Transformers.AsyncState.Class as AsyncState
import qualified Transformers.AsyncState as AsyncState
import Control.Monad.IO.Class
import GHC.Conc

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

programMtlReaderTVar :: (MonadIO m, AsyncState.MonadReaderTVar Integer m) => m Integer
programMtlReaderTVar = do
  tvar <- AsyncState.askTVar
  result <- liftIO $ atomically $ do
    n <- readTVar tvar
    if n <= 0
      then pure $ Just n
      else do
        writeTVar tvar (n - 1)
        pure Nothing
  case result of
    Nothing -> programMtlReaderTVar
    Just n -> pure n
{-# NOINLINE programMtlReaderTVar #-}

countdownMtlSTM :: Integer -> IO (Integer, Integer)
countdownMtlSTM n = do
    tvar <- newTVarIO n
    result <- flip AsyncState.runReaderTVarT tvar $ programMtlReaderTVar
    tvarResult <- readTVarIO tvar
    pure (result, tvarResult)

countdownMtlSTMDeep :: Integer -> IO (Integer, Integer)
countdownMtlSTMDeep n = do
  tvar <- newTVarIO n
  result <- runR . runR . runR . runR . runR
    . flip AsyncState.runReaderTVarT tvar
    . runR . runR . runR . runR . runR
    $ programMtlReaderTVar
  tvarResult <- readTVarIO tvar
  pure (result, tvarResult)
  where
    runR = flip M.runReaderT ()
