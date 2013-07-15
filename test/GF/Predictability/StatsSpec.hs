module GF.Predictability.StatsSpec where

import Test.Hspec
import Test.QuickCheck

import GF.Predictability.Stats -- SUT

spec :: Spec
spec = do
  describe "mean" $ do
    it "if the list contains only one element it returns that element" $
      forAll arbitrary $ \x -> mean [x] `shouldBe` x

    it "returns the mean of [1,2,3,4,5]" $
      mean [1,2,3,4,5] `shouldBe` 3
