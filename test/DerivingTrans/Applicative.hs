module DerivingTrans.Applicative where

import Control.Monad.Trans.Except qualified as T
import Test.Tasty
import Test.Tasty.QuickCheck

tests :: TestTree
tests = testGroup "Applicative"
    [ testProperty "Identity" $ lawIdentity (T.runExcept @()) (pure ())
    ]

lawIdentity ::
  forall f y a.
  (Applicative f, Eq y, Show y) =>
  (f a -> y) ->
  f a ->
  Property
lawIdentity run x = run (pure id <*> x) === run x
