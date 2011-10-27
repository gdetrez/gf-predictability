module Main where

import GF.Predictability
import GF.Predictability.Utils

-- ************************************ NOUNS ********************************
testNoun :: TestFunction
testNoun = zip

setupNoun :: SetupFunction
setupNoun ("":_) = skip $ "No singular form"
setupNoun (_:"":_) = skip $ "No plural form"
setupNoun [oeuil, yeux, gender] = do
  gender' <- case gender of
    "CommonRomance.Fem"  -> return "feminine"
    "CommonRomance.Masc" -> return "masculine"
    _ -> skip $ "invalid gender string: " ++ gender
  return
    [ [esc oeuil]
    , [esc oeuil, gender' ]
    , [esc oeuil, esc yeux, gender' ] ]
setupNoun o = skip $ "Invalid lexicon entry"
 
main = do
  let nounEx = mkExperimentWithFunctions 
               "Nouns" 
               "alltenses/ParadigmsFre.gfo"
               "mkN"
               setupNoun
               testNoun
  mainRunExperiment nounEx
