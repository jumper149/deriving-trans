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
    , bench "deep" $ nf Countdown.Mtl.countdownMtlDeep n
    , bench "shallow STM" $ nfAppIO Countdown.Mtl.countdownMtlSTM n
    , bench "deep STM" $ nfAppIO Countdown.Mtl.countdownMtlSTMDeep n
    ]
  , bgroup "deriving-trans"
    [ bench "shallow" $ nf Countdown.DerivingTrans.countdownDerivingTrans n
    , bench "deep" $ nf Countdown.DerivingTrans.countdownDerivingTransDeep n
    , bench "shallow STM" $ nfAppIO Countdown.DerivingTrans.countdownDerivingTransSTM n
    , bench "deep STM" $ nfAppIO Countdown.DerivingTrans.countdownDerivingTransSTMDeep n
    , bench "shallow stack STM" $ nfAppIO Countdown.DerivingTrans.countdownDerivingTransSTMStack n
    , bench "deep stack STM" $ nfAppIO Countdown.DerivingTrans.countdownDerivingTransSTMStackDeep n
    ]
  ]
