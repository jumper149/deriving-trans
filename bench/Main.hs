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
  , bgroup "effectful (local/static)"
    [ bench "shallow" $ nf countdownEffectfulLocal n
    , bench "deep"    $ nf countdownEffectfulLocalDeep n
    ]
  , bgroup "effectful (local/static/state)"
    [ bench "shallow" $ nf countdownEffectfulLocalSt n
    , bench "deep"    $ nf countdownEffectfulLocalDeepSt n
    ]
  , bgroup "effectful (local/static/stateM)"
    [ bench "shallow" $ nf countdownEffectfulLocalStM n
    , bench "deep"    $ nf countdownEffectfulLocalDeepStM n
    ]
  , bgroup "effectful (local/dynamic)"
    [ bench "shallow" $ nf countdownEffectfulDynLocal n
    , bench "deep"    $ nf countdownEffectfulDynLocalDeep n
    ]
  , bgroup "effectful (local/dynamic/double)"
    [ bench "shallow" $ nf countdownEffectfulDoubleDynLocal n
    , bench "deep"    $ nf countdownEffectfulDoubleDynLocalDeep n
    ]
  , bgroup "effectful (shared/static)"
    [ bench "shallow" $ nf countdownEffectfulShared n
    , bench "deep"    $ nf countdownEffectfulSharedDeep n
    ]
  , bgroup "effectful (shared/dynamic)"
    [ bench "shallow" $ nf countdownEffectfulDynShared n
    , bench "deep"    $ nf countdownEffectfulDynSharedDeep n
    ]
  , bgroup "effectful (shared/dynamic/double)"
    [ bench "shallow" $ nf countdownEffectfulDoubleDynShared n
    , bench "deep"    $ nf countdownEffectfulDoubleDynSharedDeep n
    ]
#ifdef VERSION_cleff
  , bgroup "cleff (local)"
    [ bench "shallow" $ nf countdownCleffLocal n
    , bench "deep"    $ nf countdownCleffLocalDeep n
    ]
  , bgroup "cleff (IORef)"
    [ bench "shallow" $ nf countdownCleffIORef n
    , bench "deep"    $ nf countdownCleffIORefDeep n
    ]
#endif
#ifdef VERSION_freer_simple
  , bgroup "freer-simple"
    [ bench "shallow" $ nf countdownFreerSimple n
    , bench "deep"    $ nf countdownFreerSimpleDeep n
    ]
#endif
#ifdef VERSION_eff
  , bgroup "eff"
    [ bench "shallow" $ nf countdownEff n
    , bench "deep"    $ nf countdownEffDeep n
    ]
#endif
#ifdef VERSION_mtl
  , bgroup "mtl"
    [ bench "shallow" $ nf countdownMtl n
    , bench "deep"    $ nf countdownMtlDeep n
    ]
#endif
#ifdef VERSION_fused_effects
  , bgroup "fused-effects"
    [ bench "shallow" $ nf countdownFusedEffects n
    , bench "deep"    $ nf countdownFusedEffectsDeep n
    ]
#endif
#ifdef VERSION_polysemy
  , bgroup "polysemy"
    [ bench "shallow" $ nf countdownPolysemy n
    , bench "deep"    $ nf countdownPolysemyDeep n
    ]
#endif
  , bgroup "deriving-trans"
    [ bench "shallow" $ nf countdownDerivingTrans n
    , bench "deep"    $ nf countdownDerivingTransDeep n
    ]
  ]
