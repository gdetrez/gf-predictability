module GF.Predictability.Data where

data PredictabilityResult =
  PredictabilityResult 
  { title :: String
  , preductability :: Double 
  } deriving (Show, Eq)

