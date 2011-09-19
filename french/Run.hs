module Main where

import GF.Predictability
import Text.CSV
import Debug.Trace

readNouns :: IO Lexicon
readNouns = do
  res <- parseCSVFromFile "commonNouns.csv"
  case res of
    Left error -> fail $ show error
    Right d -> return $ map (\x -> (head x, tail x)) d

testNoun :: TestFunction
testNoun = (==)

setupNoun :: SetupFunction
--setupNoun l | trace ("setupNoun " ++ show l) False = undefined
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
  where esc s = "\"" ++ s ++ "\""
setupNoun o = skip $ "Invalid lexicon entry"

main = do
  commonNouns <- readNouns
  let nounEx = mkExperimentWithFunctions 
               "Nouns" 
               "alltenses/ParadigmsFre.gfo"
               "mkN"
               commonNouns
               setupNoun
               testNoun
  mainRunExperiment nounEx
