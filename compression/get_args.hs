{- | This script is for filtering the csv lexicon according to the result experiment: it outputs the arguments sucessfully given to the paradigm (Only for the working entries)

Usage:

runghc ../get_args.hs verbs.results > verbs.filtered.csv

-}

import System
import Data.Maybe

main :: IO ()
main = do
  [resultsF] <- getArgs
  results <- readFile resultsF
  let filtered = map f (lines results)
  putStr $ unlines $ catMaybes filtered

f :: String -> Maybe String
f result = 
  let (_,r) = (read result :: (String, Either String [String]))
  in case r of 
    Left _ -> Nothing 
    Right ss -> Just $ unwords ss

 
 

