module DerivingTrans.Monad where

import Control.Monad.Trans.Except qualified as T
import Test.Tasty
import Test.Tasty.QuickCheck

tests :: TestTree
tests =
  testGroup
    "Monad"
    [ testProperty "Left Identity" $ lawLeftIdentity (T.runExcept @()) () pure
    , testProperty "Right Identity" $ lawRightIdentity (T.runExcept @()) (pure ())
    , testProperty "Associativity" $ lawAssociativity (T.runExcept @()) (pure ()) pure pure
    ]

lawLeftIdentity ::
  forall m y a b.
  (Monad m, Eq y, Show y) =>
  (m b -> y) ->
  a ->
  (a -> m b) ->
  Property
lawLeftIdentity run x f = run (return x >>= f) === run (f x)

lawRightIdentity ::
  forall m y a.
  (Monad m, Eq y, Show y) =>
  (m a -> y) ->
  m a ->
  Property
lawRightIdentity run x = run (x >>= return) === run x

lawAssociativity ::
  forall m y a b c.
  (Monad m, Eq y, Show y) =>
  (m c -> y) ->
  m a ->
  (a -> m b) ->
  (b -> m c) ->
  Property
lawAssociativity run x g f = run (x >>= (\x' -> g x' >>= f)) === run ((x >>= g) >>= f)
