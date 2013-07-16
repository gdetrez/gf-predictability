{-# LANGUAGE OverloadedStrings #-}
module GF.Predictability
  ( defaultMain
  , esc, Experiment(..)
  ) where

import GF.Predictability.Experiments
import System.Log.Logger (Priority(..))
import GF.Predictability.Options
import GF.Predictability.Logging
import GF.Predictability.HtmlReport
import Shelly

defaultMain ::  [Experiment] -> IO ()
defaultMain experiments = do
    options <- getOptions
    setLogger options
    shelly $ debug $ show options
    reports <- mapM (runExperiment options) experiments
    case (htmlReport options) of
      Just p -> shelly $ writefile p (makeHtmlReport reports)
      Nothing -> return ()
    print reports