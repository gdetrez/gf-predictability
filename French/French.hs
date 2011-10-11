module Main where

import GF.Predictability
import GF.Predictability.Utils

-- ************************************ NOUNS ********************************
testNoun :: TestFunction
testNoun = (==)

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

-- ********************************* ADJECTIVES ******************************
testAdj :: TestFunction
testAdj l1 l2 = and $ zipWith (==) l1 l2

setupAdj :: SetupFunction
setupAdj fs@[beau,beaux,belle,belles] | "" `elem` fs = skip $ "Missing form"
setupAdj [beau,beaux,belle,belles] = return
    [ [esc beau]
    , [esc beau, esc belle ]
    , [esc beau, esc belle, esc belles ] ]
--    , [esc beau, esc belle, esc beaux ] ]
setupAdj o = skip $ "Invalid lexicon entry: " ++ show o

-- ********************************** VERBS **********************************
testVerb :: TestFunction
testVerb l1 l2 = and $ zipWith (==) l1 (head l2:l2)

setupVerb :: SetupFunction
setupVerb vForms | [] `elem` vForms = skip $ "Missing form"
setupVerb vForms | length vForms /= 51 = skip "Incorrect number of forms in lexicon"
setupVerb vForms = do
  let jeter = vForms!!0
      jette = vForms!!2
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
  commonNouns <- readCSVLexicon "nouns.csv"
  adjectives <- readCSVLexicon "adjectives.csv"
  verbs <- readCSVLexicon "verbs.csv"
  let nounEx = mkExperimentWithFunctions 
               "Nouns" 
               "alltenses/ParadigmsFre.gfo"
               "mkN"
               commonNouns
               setupNoun
               testNoun
      adjEx = mkExperimentWithFunctions 
               "Adjectives" 
               "alltenses/ParadigmsFre.gfo"
               "mkA"
               adjectives
               setupAdj
               testAdj
      verbEx = mkExperimentWithFunctions 
               "Verbs" 
               "alltenses/ParadigmsFre.gfo"
               "mkV"
               verbs
               setupVerb
               testVerb
  
  mainRunExperiments [nounEx, adjEx, verbEx]
