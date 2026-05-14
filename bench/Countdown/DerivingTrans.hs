module Countdown.DerivingTrans where

import Countdown.Mtl (programMtl, programMtlReaderTVar)

import Control.Monad.Identity
import qualified Control.Monad.Trans.Reader as T
import qualified Control.Monad.Trans.State as T
import GHC.Conc

import qualified Transformers.AsyncState.Class as AsyncState
import qualified Transformers.AsyncState as AsyncState

import qualified Control.Monad.Trans.Compose as DT
import qualified Control.Monad.Trans.Compose.Infix as DT
import qualified Control.Monad.Trans.Compose.Stack as DT
import qualified Control.Monad.Trans.Compose.Transparent as DT
import qualified Control.Monad.Trans.Elevator as DT

countdownDerivingTrans :: Integer -> (Integer, Integer)
countdownDerivingTrans n = runIdentity $
  (DT.runTransparentT DT../> (`T.runStateT` n)) programMtl

countdownDerivingTransDeep :: Integer -> (Integer, Integer)
countdownDerivingTransDeep n = runIdentity $
  (DT.runTransparentT
    DT../> runR
    DT../> runR
    DT../> runR
    DT../> runR
    DT../> runR
    DT../> (`T.runStateT` n)
    DT../> runR
    DT../> runR
    DT../> runR
    DT../> runR
    DT../> runR
  )
  programMtl
  where
    runR = flip T.runReaderT ()

countdownDerivingTransSTM :: Integer -> IO (Integer, Integer)
countdownDerivingTransSTM n = do
    tvar <- newTVarIO n
    result <- (DT.runTransparentT DT../> (`AsyncState.runReaderTVarT` tvar)) programMtlReaderTVar
    tvarResult <- readTVarIO tvar
    pure (result, tvarResult)

countdownDerivingTransSTMDeep :: Integer -> IO (Integer, Integer)
countdownDerivingTransSTMDeep n = do
  tvar <- newTVarIO n
  result <-
    (DT.runTransparentT
      DT../> runR
      DT../> runR
      DT../> runR
      DT../> runR
      DT../> runR
      DT../> (`AsyncState.runReaderTVarT` tvar)
      DT../> runR
      DT../> runR
      DT../> runR
      DT../> runR
      DT../> runR
    )
    programMtlReaderTVar
  tvarResult <- readTVarIO tvar
  pure (result, tvarResult)
  where
    runR = flip T.runReaderT ()

countdownDerivingTransSTMStack :: Integer -> IO (Integer, Integer)
countdownDerivingTransSTMStack n = do
    tvar <- newTVarIO n
    result <- DT.runStackT (DT.RunNilT DT.:..> (`AsyncState.runReaderTVarT` tvar)) programMtlReaderTVar
    tvarResult <- readTVarIO tvar
    pure (result, tvarResult)

countdownDerivingTransSTMStackDeep :: Integer -> IO (Integer, Integer)
countdownDerivingTransSTMStackDeep n = do
  tvar <- newTVarIO n
  result <-
    DT.runStackT
      (DT.RunNilT
        DT.:..> runR
        DT.:..> runR
        DT.:..> runR
        DT.:..> runR
        DT.:..> runR
        DT.:..> (`AsyncState.runReaderTVarT` tvar)
        DT.:..> runR
        DT.:..> runR
        DT.:..> runR
        DT.:..> runR
        DT.:..> runR
      )
    programMtlReaderTVar
  tvarResult <- readTVarIO tvar
  pure (result, tvarResult)
  where
    runR = flip T.runReaderT ()
