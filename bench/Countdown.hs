{-# LANGUAGE CPP #-}
module Countdown where

import Control.Monad.ST
import Data.STRef

-- mtl
#ifdef VERSION_mtl
import qualified Control.Monad.Reader as M
import qualified Control.Monad.State as M
import Data.Functor.Identity
#endif

import qualified Control.Monad.Trans.Compose as DT
import qualified Control.Monad.Trans.Compose.Infix as DT
import qualified Control.Monad.Trans.Compose.Transparent as DT
import qualified Control.Monad.Trans.Elevator as DT

----------------------------------------
-- reference

countdownRef :: Integer -> (Integer, Integer)
countdownRef n = if n <= 0 then (n, n) else countdownRef $ n - 1
{-# NOINLINE countdownRef #-}

----------------------------------------
-- ST

programST :: STRef s Integer -> ST s Integer
programST ref = do
  n <- readSTRef ref
  if n <= 0
    then pure n
    else do
      writeSTRef ref $! n - 1
      programST ref
{-# NOINLINE programST #-}

countdownST :: Integer -> (Integer, Integer)
countdownST n = runST $ do
  ref <- newSTRef n
  a <- programST ref
  s <- readSTRef ref
  pure (a, s)

----------------------------------------
-- mtl

#ifdef VERSION_mtl

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

#endif

----------------------------------------
-- deriving-trans

countdownDerivingTrans :: Integer -> (Integer, Integer)
countdownDerivingTrans n = runIdentity $
  (DT.runTransparentT DT../> (`M.runStateT` n)) programMtl

countdownDerivingTransDeep :: Integer -> (Integer, Integer)
countdownDerivingTransDeep n = runIdentity $
  (DT.runTransparentT
    DT../> runR
    DT../> runR
    DT../> runR
    DT../> runR
    DT../> runR
    DT../> (`M.runStateT` n)
    DT../> runR
    DT../> runR
    DT../> runR
    DT../> runR
    DT../> runR
  )
  programMtl
  where
    runR = flip M.runReaderT ()
