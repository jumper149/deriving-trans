module Main (main) where

import Test.Tasty.Bench

import Countdown

main :: IO ()
main = defaultMain
  [ bgroup "countdown" $ map countdown [1000, 2000, 3000]
  ]
