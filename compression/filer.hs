{- | This script is for filtering the csv lexicon according to the result experiment: it outputs the lines from the input lexicon which passed the experiment.
Usage:

runghc ../filer.hs verbs.csv verbs.results > verbs.filtered.csv

-}

import System
import Data.Maybe

main :: IO ()
main = do
  [csvF,resultsF] <- getArgs
  csv <- readFile csvF
  results <- readFile resultsF
  let filtered = map f $ zip (lines csv) (lines results)
  putStr $ unlines $ catMaybes filtered

f :: (String, String) -> Maybe String
f (entry, result) = 
  let (_,r) = (read result :: (String, Either String [String]))
  in case r of 
    Left _ -> Nothing 
    Right _ -> Just entry


