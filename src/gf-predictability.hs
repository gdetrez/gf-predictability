{-# LANGUAGE OverloadedStrings #-}
module Main where

import Experiments
import System.Log.Logger (Priority(..))
import GF.Predictability.Options
import GF.Predictability.Logging
import GF.Predictability.HtmlReport
import Shelly

main :: IO ()
main = do
    options <- getOptions
    setLogger options
    shelly $ debug $ show options
    reports <- mapM (runExperiment options) demoExperiments
    case (htmlReport options) of
      Just p -> shelly $ writefile p (makeHtmlReport reports)
      Nothing -> return ()
    print reports


demoExperiments :: [Experiment]
demoExperiments =
  [ Experiment  { title = "English nouns"
  , lexicon = "test/LexiconEng.gfo"
  , category = "N"
  , morphology = "test/ParadigmEng.gfo"
  , smartparadigm = "mkN"
  , setup = \[cat,cats] -> [[esc cat],[esc cat,esc cats]] }
  , Experiment  { title = "English verbs"
  , lexicon = "test/LexiconEng.gfo"
  , category = "V"
  , morphology = "test/ParadigmEng.gfo"
  , smartparadigm = "mkV"
  , setup = \[eat,ate,eaten] ->
      [ [esc eat]
      , [esc eat, esc ate]
      , [esc eat, esc ate, esc eaten]] }
  ]

experiments :: [Experiment]
experiments =
  [ Experiment  { title = "English nouns"
  , lexicon = "/home/gregoire/GF/lib/src/english/DictEng.gf"
  , category = "N"
  , morphology = "alltenses/ParadigmsEng.gfo"
  , smartparadigm = "mkN"
  , setup = \(man:_:men:_:_) -> [ [esc man], [esc man,esc men] ] }
  , Experiment  { title = "English verbs"
  , lexicon = "/home/gregoire/GF/lib/src/english/DictEng.gf"
  , category = "V"
  , morphology = "alltenses/ParadigmsEng.gfo"
  , smartparadigm = "mkV"
  , setup = \(blow:blows:blown:blowing:blew:_) ->
        [ [ esc blow ]
        , [ esc blow, esc blew ]
        , [ esc blow, esc blew, esc blown ]
        , [ esc blow, esc blows, esc blew, esc blown, esc blowing ] ] }
  ]
--  tmain = mapM runExperiment experiments >>= print
