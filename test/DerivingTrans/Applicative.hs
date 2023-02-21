module DerivingTrans.Applicative where

import Control.Monad.Trans.Except qualified as T
import Test.Tasty
import Test.Tasty.QuickCheck

tests :: TestTree
tests =
  testGroup
    "Applicative"
    [ testProperty "Identity" $ lawIdentity (T.runExcept @()) (pure ())
    , testProperty "Composition" $ lawComposition (T.runExcept @()) (pure ()) (pure id) (pure id)
    , testProperty "Homomorphism" $ lawHomomorphism (T.runExcept @()) () id
    , testProperty "Interchange" $ lawInterchange (T.runExcept @()) () (pure id)
    ]

lawIdentity ::
  forall f y a.
  (Applicative f, Eq y, Show y) =>
  (f a -> y) ->
  f a ->
  Property
lawIdentity run x = run (pure id <*> x) === run x

lawComposition ::
  forall f y a b c.
  (Applicative f, Eq y, Show y) =>
  (f c -> y) ->
  f a ->
  (f (a -> b)) ->
  (f (b -> c)) ->
  Property
lawComposition run x g f = run (pure (.) <*> f <*> g <*> x) === run (f <*> (g <*> x))

lawHomomorphism ::
  forall f y a b.
  (Applicative f, Eq y, Show y) =>
  (f b -> y) ->
  a ->
  (a -> b) ->
  Property
lawHomomorphism run x f = run (pure f <*> pure x) === run (pure (f x))

lawInterchange ::
  forall f y a b.
  (Applicative f, Eq y, Show y) =>
  (f b -> y) ->
  a ->
  (f (a -> b)) ->
  Property
lawInterchange run x f = run (f <*> pure x) === run (pure ($ x) <*> f)
