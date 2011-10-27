module Main where

import GF.Predictability
import GF.Predictability.Utils
import Debug.Trace

-- ********************************** VERBS **********************************
test :: TestFunction
test [nice, _, nicer, _, nicest, _, _] [nice', nicer', nicest'] = 
  [(nice, nice'), (nicer,nicer'), (nicest,nicest')] 
test l1 l2 | trace (show l1) False = undefined

setup :: SetupFunction
setup vForms | "-" `elem` vForms = skip $ "Missing form"
setup [nice, nicer, nicest] = 
  return $ map (map esc )
    [ [ nice ]
    , [ nice, nicer ]
    ]
 
main = do
  let ex = mkExperimentWithFunctions 
           "English adjectives"
           "alltenses/ParadigmsEng.gfo"
           "mkA"
           setup
           test
  
  mainRunExperiment ex
