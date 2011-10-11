module Main where

import GF.Predictability
import GF.Predictability.Utils

-- ********************************* ADJECTIVES ******************************
testAdj :: TestFunction
testAdj = take 4

setupAdj :: SetupFunction
setupAdj fs@[beau,beaux,belle,belles] | "" `elem` fs = skip $ "Missing form"
setupAdj [beau,beaux,belle,belles] = return
    [ [esc beau]
    , [esc beau, esc belle ]
    , [esc beau, esc belle, esc beaux ] ]
--    , [esc beau, esc belle, esc beaux ] ]
setupAdj o = skip $ "Invalid lexicon entry: " ++ show o
 
main = do
  let adjEx = mkExperimentWithFunctions 
               "Adjectives" 
               "alltenses/ParadigmsFre.gfo"
               "mkA"
               setupAdj
               testAdj
  mainRunExperiment adjEx
