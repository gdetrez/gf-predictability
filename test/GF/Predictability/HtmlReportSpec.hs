module GF.Predictability.HtmlReportSpec where

import Test.Hspec
import Test.QuickCheck
--import Control.Monad (liftM)
--import Data.List (sort)
import GF.Predictability.Experiments
import Text.Blaze.Html.Renderer.String
import Data.List (isInfixOf)

import GF.Predictability.HtmlReport -- SUT

spec :: Spec
spec = do
  let testReport = ExperimentReport
          { experiment   = "Test Experiment"
          , entries      = 42
          , meanCost     = 1.42
          , medianCost   = 2
          , distribution = [38, 2, 1, 1] }

  describe "experimentTR" $ do

    it "has a cell with the number of entries" $
      experimentTR testReport `shouldContain` "<td>42</td>"


    it "create a <tr> element" $
      take 4 (renderHtml (experimentTR testReport)) `shouldBe` "<tr>"

    it "has a cell with the experiment title" $
      experimentTR testReport `shouldContain` "<td>Test Experiment</td>"

    it "has a cell with the number of entries" $
      experimentTR testReport `shouldContain` "<td>42</td>"

    it "has a cell with the mean cost" $
      experimentTR testReport `shouldContain` "<td>1.42</td>"

    it "has a cell with the median cost" $
      experimentTR testReport `shouldContain` "<td>2.0</td>"

    it "has cells with m1 and m2" $
      experimentTR testReport
        `shouldContain` "<td><abbr title=\"38\">90%</abbr></td><td><abbr title=\"40\">95%</abbr></td>"

    it "has a cell containing an image" $
      experimentTR testReport `shouldContain` "<td><img"

  describe "htmlReport" $ do

    it "has a <title> tag" $
      reportHtml [testReport]
        `shouldContain` "<title>GF predictability report</title>"

    it "has a h1 tag" $
      reportHtml [testReport]
        `shouldContain` "<h1>GF predictability report</h1>"

  describe "distributionSparkline" $
    it "starts with data:image/png;base64" $
      take 21 (distributionSparkline testReport)
        `shouldBe` "data:image/png;base64"




shouldContain html substr = (renderHtml html) `shouldSatisfy` (isInfixOf substr)
