module Countdown.ST where

import Control.Monad.ST
import Data.STRef

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
