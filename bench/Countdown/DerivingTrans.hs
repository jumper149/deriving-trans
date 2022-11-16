module Countdown.DerivingTrans where

import Countdown.Mtl (programMtl)

import Control.Monad.Identity
import qualified Control.Monad.Trans.Reader as T
import qualified Control.Monad.Trans.State as T

import qualified Control.Monad.Trans.Compose as DT
import qualified Control.Monad.Trans.Compose.Infix as DT
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
