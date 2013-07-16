{-# LANGUAGE OverloadedStrings #-}
module Main where

import GF.Predictability

main = defaultMain myExperiments

myExperiments :: [Experiment]
myExperiments =
  [ Experiment  { title = "English nouns"
  , lexicon       = "test/LexiconEng.gfo"
  , category      = "N"
  , nforms        = 2
  , morphology    = "test/ParadigmEng.gfo"
  , smartparadigm = "mkN"
  , setup         = \[cat,cats] -> [[esc cat],[esc cat,esc cats]] }
  , Experiment  { title = "English verbs"
  , lexicon       = "test/LexiconEng.gfo"
  , category      = "V"
  , nforms        = 3
  , morphology    = "test/ParadigmEng.gfo"
  , smartparadigm = "mkV"
  , setup         = \[eat,ate,eaten] ->
      [ [esc eat]
      , [esc eat, esc ate]
      , [esc eat, esc ate, esc eaten]] }
  ]
