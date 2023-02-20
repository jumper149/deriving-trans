module DerivingTrans where

import Test.Tasty
import Test.Tasty.QuickCheck

main :: IO ()
main =
  defaultMain $
    testProperty "example" $ (1 :: Int) == 1
