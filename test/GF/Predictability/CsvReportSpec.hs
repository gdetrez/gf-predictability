{-# LANGUAGE OverloadedStrings #-}
module GF.Predictability.CsvReportSpec where

import Test.Hspec
import Test.QuickCheck
import GF.Predictability.Experiments
import Data.List (isInfixOf)
import Data.Text (Text)
import qualified Data.Text as T

import GF.Predictability.CsvReport -- SUT

spec :: Spec
spec = do
  let testReports =
        [ ExperimentReport "Experiment 1" 42 4.2 2 [38, 2, 1, 1]
        , ExperimentReport "Experiment 2" 43 3 2 [38, 2, 1, 1]
        , ExperimentReport "Experiment 3" 44 1.09 2 [38, 2, 1, 1]
        , ExperimentReport "Final experiment" 189 1.43 2 [38, 2, 1, 1] ]

  describe "makeCsvReport" $ do
    it "has 2 lines" $
      length (T.lines (makeCsvReport testReports)) `shouldBe` 2

    it "builds the header (1st line) from the experiment titles" $
      let header:_ = T.lines (makeCsvReport testReports) in
      header `shouldBe` "Experiment 1,Experiment 2,Experiment 3,Final experiment"

    it "builds the data (2nd line) from the experiments' mean costs" $
      let _:data_:_ = T.lines (makeCsvReport testReports) in
      data_ `shouldBe` "4.2,3.0,1.09,1.43"
