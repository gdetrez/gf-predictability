module Main where

import GF.Predictability
import GF.Predictability.Utils

-- ********************************** VERBS **********************************
testVerb :: TestFunction
testVerb l1 l2 = zip l1 l2

setupVerb :: SetupFunction
setupVerb vForms | [] `elem` vForms = skip $ "Missing form"
setupVerb [blow,blows,blown,blowing,blew] = 
  return $ map (map esc )
    [ [ blow ]
    , [ blow, blew ]
    , [ blow, blew, blown ]
    , [ blow, blows, blew, blown, blowing ]
    ]
 
main = do
  let verbEx = mkExperimentWithFunctions 
               "English verbs" 
               "alltenses/ParadigmsEng.gfo"
               "mkV"
               setupVerb
               testVerb
  
  mainRunExperiment verbEx
