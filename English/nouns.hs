module Main where

import GF.Predictability
import GF.Predictability.Utils
import Debug.Trace
-- ********************************** VERBS **********************************
test :: TestFunction
test [man, _, men, _, _] l2 = zip [man, men] l2

setup :: SetupFunction
setup vForms | [] `elem` vForms = skip $ "Missing form"
setup [man, men] = 
  return $ map (map esc )
    [ [ man ]
    , [ man, men ]
    ]
 
main = do
  let ex = mkExperimentWithFunctions 
           "English nouns" 
           "alltenses/ParadigmsEng.gfo"
           "mkN"
           setup
           test
  
  mainRunExperiment ex
