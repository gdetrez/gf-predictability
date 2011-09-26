module Main where


import GF.Predictability
import GF.Predictability.Utils

-- ************************************ NOUNS *******************************
testNoun :: TestFunction
testNoun = (==)

setupNoun :: SetupFunction
setupNoun fs | "" `elem` fs = skip $ "Missing form"
setupNoun [gurka,gurkas,gurkan,gurkans,gurkor,gurkors,gurkorna,gurkornas,_] =
  return [ [ esc gurka ]
         , [ esc gurka, esc gurkor ]
         , [ esc gurka, esc gurkan , esc gurkor, esc gurkorna ] ]
setupNoun o = skip $ "Invalid lexicon entry"

  
main :: IO ()
main = do
  nouns <- readCSVLexicon "nouns.csv"
  let nounEx = mkExperimentWithFunctions 
               "Nouns" 
               "alltenses/ParadigmsSwe.gfo"
               "mkN"
               nouns
               setupNoun
               testNoun
  mainRunExperiments [nounEx]
