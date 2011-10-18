module Main where


import GF.Predictability
import GF.Predictability.Utils

-- ******************************* ADJECTIVES *******************************
testAdjective :: TestFunction
testAdjective gf lex = zip (take 16 gf) lex

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
  let adjectiveEx = mkExperimentWithFunctions 
               "Adjectives"
               "alltenses/ParadigmsSwe.gfo"
               "mkA"
               setupAdjective
               testAdjective
  mainRunExperiment adjectiveEx
