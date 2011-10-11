module Main where


import GF.Predictability
import GF.Predictability.Utils

-- ************************************ NOUNS *******************************
testNoun :: TestFunction
testNoun = (==) . take 8

setupNoun :: SetupFunction
setupNoun fs | "" `elem` fs = skip $ "Missing form"
setupNoun [gurka,gurkas,gurkan,gurkans,gurkor,gurkors,gurkorna,gurkornas] =
  return [ [ esc gurka ]
         , [ esc gurka, esc gurkor ]
         , [ esc gurka, esc gurkan , esc gurkor, esc gurkorna ] ]
setupNoun o = skip $ "Invalid lexicon entry"

main :: IO ()
main = do
  let nounEx = mkExperimentWithFunctions 
               "Nouns" 
               "alltenses/ParadigmsSwe.gfo"
               "mkN"
               setupNoun
               testNoun
  mainRunExperiment nounEx
