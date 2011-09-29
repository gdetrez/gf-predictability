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

  
-- ******************************* ADJECTIVES *******************************
testAdjective :: TestFunction
testAdjective = (==) . take 16

setupAdjective :: SetupFunction
setupAdjective fs | "" `elem` fs = skip $ "Missing form"
setupAdjective [liten, litens, litet, litets, sma_undef, smas_undef,
           lilla, lillas, sma_def, smas_def, 
           mindre, mindres, minst, minsts, minsta, minstas ] =
  return [ [ esc liten ]
         , [ esc liten, esc litet ]
         , [ esc liten, esc mindre, esc minst ]
         , [ esc liten, esc litet, esc sma_def, esc mindre, esc minst ]
         , [ esc liten,esc litet,esc lilla,esc sma_def,esc mindre,esc minst,
             esc minsta ]]
setupAdjective o = skip $ "Invalid lexicon entry"

  
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
  adjectives <- readCSVLexicon "adjectives.csv"
  let adjectiveEx = mkExperimentWithFunctions 
               "Adjectives"
               "alltenses/ParadigmsSwe.gfo"
               "mkA"
               adjectives
               setupAdjective
               testAdjective
  mainRunExperiments [nounEx, adjectiveEx]
