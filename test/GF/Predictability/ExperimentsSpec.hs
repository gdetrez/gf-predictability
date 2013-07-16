{-# LANGUAGE OverloadedStrings #-}
module GF.Predictability.ExperimentsSpec where

import Test.Hspec
import Test.QuickCheck
import Shelly
import GF.Predictability.Options
import Control.Monad (liftM)
import Data.List (sort,isPrefixOf,isInfixOf)

import GF.Predictability.Experiments -- SUT

spec :: Spec
spec = do
  describe "makeReport" $ do
    let dummyExp = undefined :: Experiment

    it "counts the number of entries" $
      forAll arbitrary $ \is -> entries (makeReport dummyExp is) `shouldBe` length is

    let exampleCosts = [1,2,3,4,5,1,2,3,4,1,2,3,1,2,1]

    it "compute the mean cost" $
      meanCost (makeReport dummyExp exampleCosts) `shouldBe` 2.3333333333333335

    it "compute the median" $
      medianCost (makeReport dummyExp exampleCosts) `shouldBe` 2

    it "compute the number of ones" $ 
      m1 (makeReport dummyExp exampleCosts) `shouldBe` 5

    it "compute the number of elements with a cost of 1 or 2" $
      m2 (makeReport dummyExp exampleCosts) `shouldBe` 9

    it "returns a break down of the costs distribution" $
      distribution (makeReport dummyExp exampleCosts) `shouldBe` [5,4,3,2,1]

    it "has a mean between bellow the maximum" $ property $
      forAll getCosts $ \xs ->
        meanCost (makeReport dummyExp xs) `shouldSatisfy` (<= fromIntegral (maximum xs))

    it "m1 is <= that the list size" $ property $
      forAll getCosts $ \xs ->
        m1 (makeReport dummyExp xs) `shouldSatisfy` (<= length xs)

    it "m2 is <= that the list size" $ property $
      forAll getCosts $ \xs ->
        m2 (makeReport dummyExp xs) `shouldSatisfy` (<= length xs)

    it "m1 <= m2" $
      forAll getCosts $ \xs ->
        m1 (makeReport dummyExp xs) <= m2 (makeReport dummyExp xs) `shouldBe` True

    it "the distribution sums to the length of the list" $
      forAll getCosts $ \xs ->
        sum (distribution (makeReport dummyExp xs)) `shouldBe` length xs

  describe "ppReport" $ do
    let report = ExperimentReport { experiment   = "English verbs"
                                  , entries      = 3
                                  , meanCost     = 2
                                  , medianCost   = 2
                                  , distribution = [1,1,1] }

    it "Should start with the title" $
      head (lines (ppReport report))
        `shouldSatisfy` isPrefixOf "English verbs"
    it "prints the mean cost" $
      ppReport report
        `shouldSatisfy` isInfixOf "mean cost: 2.0"
    it "prints the median cost" $
      ppReport report
        `shouldSatisfy` isInfixOf "median cost: 2.0"
    it "prints the m=1 value" $
      ppReport report
        `shouldSatisfy` isInfixOf "m=1: 33% (1)"
    it "prints the m<=2 value" $
      ppReport report
        `shouldSatisfy` isInfixOf "m<=2: 67% (2)"

  describe "getLexicon" $ do
    context "Toy grammar LexiconEng" $ do
      it "get the lexicon for the V category" $
        liftM sort (shelly $ getLexicon "gf" "test/LexiconEng.gf" "V" 3)
          `shouldReturn` [["eat","ate","eaten"],["love","loved","loved"],["pray","prayed","prayed"]]

      it "gets the lexicon for the N category" $
        liftM sort (shelly $ getLexicon "gf" "test/LexiconEng.gf" "N" 2)
          `shouldReturn` [["cat","cats"],["dog","dogs"],["mouse","mices"]]

  describe "computeConcrete" $ do
    context "with the toy morphology" $ do

      it "uses mkN with 1 argument" $
        shelly (computeConcrete "gf" "test/ParadigmEng.gf" "mkN" ["\"mouse\""])
          `shouldReturn` ["mouse","mouses"]
      it "uses mkN with 2 argument" $
        shelly (computeConcrete "gf" "test/ParadigmEng.gf" "mkN" ["\"mouse\"","\"mices\""])
          `shouldReturn` ["mouse","mices"]

      it "uses mkV with 1 argument" $
        shelly (computeConcrete "gf" "test/ParadigmEng.gf" "mkV" ["\"eat\""])
          `shouldReturn` ["eat", "eated","eated"]
      it "uses mkV with 2 argument" $
        shelly (computeConcrete "gf" "test/ParadigmEng.gf" "mkV" ["\"eat\"","\"ate\""])
          `shouldReturn` ["eat", "ate","ate"]
      it "uses mkV with 3 argument" $
        shelly (computeConcrete "gf" "test/ParadigmEng.gf" "mkV" ["\"eat\"","\"ate\"","\"eaten\""])
          `shouldReturn` ["eat", "ate","eaten"]
  -- ~~~ Small experiments (integration tests) ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  context "Toy experiment with english nouns" $ do
    let experiment = Experiment { title = "English nouns"
                                , lexicon = "test/LexiconEng.gfo"
                                , category = "N"
                                , nforms        = 2
                                , morphology = "test/ParadigmEng.gfo"
                                , smartparadigm = "mkN"
                                , setup = \[cat,cats] -> [[esc cat],[esc cat,esc cats]] }
    describe "word costs" $ do
        it "cat has cost 1" $
          shelly (wordCost "gf" experiment ["cat", "cats"]) `shouldReturn` 1
        it "dog has cost 1" $
          shelly (wordCost "gf" experiment ["dog", "dogs"]) `shouldReturn` 1
        it "mouse has cost 2" $
          shelly (wordCost "gf" experiment ["mouse", "mices"]) `shouldReturn` 2
    describe "runExperiment" $ do
      let report = ExperimentReport
                        { experiment   = "English nouns"
                        , entries      = 3
                        , meanCost     = 1.3333333333333333
                        , medianCost   = 1
                        , distribution = [2,1] }

      it "should produce the expected report" $
        runExperiment defaultOptions experiment `shouldReturn` report

  context "Toy experiment with english verbs" $ do
    let experiment = Experiment { title = "English verbs"
                                , lexicon = "test/LexiconEng.gfo"
                                , category = "V"
                                , nforms        = 3
                                , morphology = "test/ParadigmEng.gfo"
                                , smartparadigm = "mkV"
                                , setup = \[eat,ate,eaten] ->
                                    [ [esc eat]
                                    , [esc eat, esc ate]
                                    , [esc eat, esc ate, esc eaten]] }
    describe "word costs" $ do
        it "pray has cost 1" $
          shelly (wordCost "gf" experiment ["pray", "prayed", "prayed"]) `shouldReturn` 1
        it "love has cost 2" $
          shelly (wordCost "gf" experiment ["loved", "loved", "loved"]) `shouldReturn` 2
        it "eat has cost 3" $
          shelly (wordCost "gf" experiment ["eat","ate","eaten"]) `shouldReturn` 3
    describe "runExperiment" $ do
      let report = ExperimentReport { experiment   = "English verbs"
                                    , entries      = 3
                                    , meanCost     = 2
                                    , medianCost   = 2
                                    , distribution = [1,1,1] }

      it "should produce the expected report" $
        runExperiment defaultOptions experiment `shouldReturn` report

getCosts :: Gen [Int]
getCosts = listOf1 (elements [1..100])
