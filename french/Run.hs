module Main where

import GF.Predictability
import Text.CSV

readNouns :: IO [[String]]
readNouns = do
  res <- parseCSVFromFile "commonNouns-small.csv"
  case res of
    Left error -> fail $ show error
    Right d -> return d

main = do
  commonNouns <- readNouns
  let nounEx = mkExperiment "Nouns" 
                            "alltenses/ParadigmsFre.gfo"
                            "mkN"
                            commonNouns
  mainRunExperiment nounEx
