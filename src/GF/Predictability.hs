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
import GF.Predictability.CsvReport
import Shelly

defaultMain ::  [Experiment] -> IO ()
defaultMain experiments = do
    options <- getOptions
    setLogger options
    shelly $ silently $ do
      debug $ show options
      reports <- mapM (runExperiment options) experiments
      case htmlReport options of
        Just p  -> writefile p (makeHtmlReport reports)
        Nothing -> return ()
      case csvReport options of
        Just p  -> writefile p (makeCsvReport reports)
        Nothing -> return ()
      notice ""
      notice $ take 36 (~~~) ++ " Results " ++ take 36 (~~~)
      mapM_ (notice . ppReport) reports
      notice (take 79 (~~~))
  where (~~~) = repeat '~'
