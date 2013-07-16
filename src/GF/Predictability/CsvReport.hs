{-# LANGUAGE OverloadedStrings #-}
module GF.Predictability.CsvReport where

import GF.Predictability.Experiments
import Data.Text (Text)
import qualified Data.Text as T

makeCsvReport :: [ExperimentReport] -> Text
makeCsvReport rs = T.unlines
  [ T.intercalate "," (map (T.pack . experiment) rs)
  , T.intercalate "," (map (T.pack . show . meanCost) rs) ]
