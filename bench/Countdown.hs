module Countdown where

import qualified Countdown.DerivingTrans
import qualified Countdown.Mtl
import qualified Countdown.Reference
import qualified Countdown.ST

import Test.Tasty.Bench

countdown :: Integer -> Benchmark
countdown n = bgroup (show n)
  [ bench "reference (pure)" $ nf Countdown.Reference.countdownRef n
  , bench "reference (ST)" $ nf Countdown.ST.countdownST n
  , bgroup "mtl"
    [ bench "shallow" $ nf Countdown.Mtl.countdownMtl n
    , bench "deep"    $ nf Countdown.Mtl.countdownMtlDeep n
    ]
  , bgroup "deriving-trans"
    [ bench "shallow" $ nf Countdown.DerivingTrans.countdownDerivingTrans n
    , bench "deep"    $ nf Countdown.DerivingTrans.countdownDerivingTransDeep n
    ]
  ]
