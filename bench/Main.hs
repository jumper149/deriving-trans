{-# LANGUAGE CPP #-}

module Main (main) where

import Test.Tasty.Bench

import Countdown

main :: IO ()
main = defaultMain
  [ bgroup "countdown" $ map countdown [1000, 2000, 3000]
  ]

countdown :: Integer -> Benchmark
countdown n = bgroup (show n)
  [ bench "reference (pure)"             $ nf countdownRef n
  , bench "reference (ST)"               $ nf countdownST n
  , bgroup "mtl"
    [ bench "shallow" $ nf countdownMtl n
    , bench "deep"    $ nf countdownMtlDeep n
    ]
  , bgroup "deriving-trans"
    [ bench "shallow" $ nf countdownDerivingTrans n
    , bench "deep"    $ nf countdownDerivingTransDeep n
    ]
  ]
