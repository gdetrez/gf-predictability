module Main where

import GF.Predictability
import GF.Predictability.Utils
import Debug.Trace
-- ********************************** VERBS **********************************
test :: TestFunction
test l1 l2 = zip l1 l2

setup :: SetupFunction
setup vForms | [] `elem` vForms = skip $ "Missing form"
setup [talo,talon,taloa,talona,taloon,talojen,taloja,taloina,taloissa,taloihin] = 
  return $ map (map esc )
    [ [ talo ]
    , [ talo, taloja ]
    , [ talo, talon, taloja ]
    , [ talo, talon, taloja, taloa ]
    , [ talo,talon,taloa,talona,taloon,talojen,taloja,taloina,taloissa,taloihin ]

    ]
setup _ = skip $ "Missing form"
 
main = do
  let ex = mkExperimentWithFunctions 
           "Finnish nouns" 
           "alltenses/ShortParadigmsFin.gfo"
           "mkNForms"
           setup
           test
  
  mainRunExperiment ex
