{-# LANGUAGE OverloadedStrings #-}
module Main where

import Experiments
import System.Log.Logger (Priority(..))
import GF.Predictability.Options
import GF.Predictability.Logging
import Shelly

main :: IO ()
main = do
    options <- getOptions
    setLogger options
    shelly $ debug $ show options
    mapM (runExperiment options) experiments >>= print

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
