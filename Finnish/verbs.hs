module Main where

import GF.Predictability
import GF.Predictability.Utils

-- ********************************** VERBS **********************************
testVerb :: TestFunction
testVerb l1 l2 = zip l1 l2

setupVerb :: SetupFunction
setupVerb vForms | [] `elem` vForms = skip $ "Missing form"
setupVerb [panna,panen,panee,panevat,pankaa,pannaan,panin,pani,panisi,pannut,pantu,pannee] = 
  return $ map (map esc )
    [ [ panna ]
    , [ panna, pani ]
    , [ panna, panen, pani ]
    , [ panna, panen, panee, panevat, pankaa, pannaan, panin, pani, panisi, pannut, pantu, pannee ]
    ]
 
main = do
  let verbEx = mkExperimentWithFunctions 
               "Finnish verbs" 
               "alltenses/ShortParadigmsFin.gfo"
               "mkVForms"
               setupVerb
               testVerb
  
  mainRunExperiment verbEx
