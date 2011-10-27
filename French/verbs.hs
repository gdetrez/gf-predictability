module Main where

import GF.Predictability
import GF.Predictability.Utils

-- ********************************** VERBS **********************************
testVerb :: TestFunction
testVerb l1 l2= zip (take 51 $ drop 1 l1) l2

setupVerb :: SetupFunction
setupVerb vForms | [] `elem` vForms = skip $ "Missing form"
setupVerb vForms | length vForms /= 51 = skip "Incorrect number of forms in lexicon"
setupVerb vForms = do
  let jeter = vForms!!0
      jette = vForms!!3
      jettera = vForms!!33
      tenir = vForms!!0
      tiens = vForms!!1
      tient = vForms!!3
      tenons = vForms!!4
      tenez = vForms!!5
      tiennent = vForms!!6
      tienne = vForms!!7
      tenions = vForms!!10
      tiensI = vForms!!43
      tint = vForms!!27
      tiendra = vForms!!33
      tenu = vForms!!46
  return $ map (map esc )
    [ [ jeter ]
    , [ jeter, jette, jettera ]
    , [ tenir, tiens, tenons, tiennent, tint, tiendra, tenu ]
    , [ tenir,tiens,tient,tenons,tenez,tiennent,tienne,tenions,tiensI,tint,tiendra,tenu ]
    ]
 
main = do
  let verbEx = mkExperimentWithFunctions 
               "French Verbs" 
               "alltenses/ParadigmsFre.gfo"
               "mkV"
               setupVerb
               testVerb
  
  mainRunExperiment verbEx
