module Main where


import GF.Predictability
import GF.Predictability.Utils

-- *********************************** VERBS *********************************
testVerb :: TestFunction
testVerb = (==) . prune
  where prune [ går,gås,gick,gicks,
                gå_imp,_,gå_inf,gås_inf,gått,gåtts,
                gången,gångens,gånget,gångets,
                gångna_undef,gångnas_undef,gångna_def,gångnas_def, 
                _,_,_,_ ] = [ går,gås,gick,gicks,
                        gå_imp,gå_inf,gås_inf,gått,gåtts,
                        gången,gångens,gånget,gångets,
                        gångna_undef,gångnas_undef,gångna_def,gångnas_def ]
        prune _ = []
setupVerb :: SetupFunction
setupVerb fs | "" `elem` fs = skip $ "Missing form"
setupVerb [ går,gås,gick,gicks,
            gå_imp,gå_inf,gås_inf,gått,gåtts,
            gången,gångens,gånget,gångets,
            gångna_undef,gångnas_undef,gångna_def,gångnas_def ] =
  return [ [ esc går ]
         , [ esc gå_inf, esc gick, esc gått ]
         , [ esc gå_inf, esc går, esc gå_imp, esc gick, esc gått, esc gången ] ]
setupVerb o = skip $ "Invalid lexicon entry"

  
main :: IO ()
main = do
  let verbEx = mkExperimentWithFunctions 
               "Verbs"
               "alltenses/ParadigmsSwe.gfo"
               "mkV"
               setupVerb
               testVerb
  mainRunExperiment verbEx
