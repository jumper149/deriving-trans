module DerivingTrans.Functor where

import Control.Monad.Trans.Except qualified as T
import Test.Tasty
import Test.Tasty.QuickCheck

tests :: TestTree
tests = testGroup "Functor"
    [ testProperty "Identity" $ lawIdentity (T.runExcept @()) (pure ())
    ]

lawIdentity ::
  forall f y a.
  (Functor f, Eq y, Show y) =>
  (f a -> y) ->
  f a ->
  Property
lawIdentity run x = run (fmap id x) === run (id x)
