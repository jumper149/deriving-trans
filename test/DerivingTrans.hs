module DerivingTrans where

import DerivingTrans.Applicative qualified
import DerivingTrans.Functor qualified
import DerivingTrans.Monad qualified
import Test.Tasty

main :: IO ()
main =
  defaultMain $
    testGroup
      "deriving-trans"
      [ DerivingTrans.Functor.tests
      , DerivingTrans.Applicative.tests
      , DerivingTrans.Monad.tests
      ]
