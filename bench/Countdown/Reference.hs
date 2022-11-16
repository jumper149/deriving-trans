module Countdown.Reference where

countdownRef :: Integer -> (Integer, Integer)
countdownRef n = if n <= 0 then (n, n) else countdownRef $ n - 1
{-# NOINLINE countdownRef #-}
