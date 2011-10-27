module Main where
{- This experiment is almost the same as the verbs experiment but for the one place mkV smart paradigm, instead of giving the infinitive, as it is traditionaly done, we give the present tense. It seems that this makes it easier to guess the correct paradigm, this experiment tries to confirm that. -}

import GF.Predictability
import GF.Predictability.Utils

-- *********************************** VERBS *********************************
testVerb :: TestFunction
testVerb [ går,gås,gick,gicks,
           gå_imp,_,gå_inf,gås_inf,gått,gåtts,
           gången,gångens,gånget,gångets,
           gångna_undef,gångnas_undef,gångna_def,gångnas_def, 
           _,_,_,_ ] lex =
  zip [ går,gås,gick,gicks,
        gå_imp,gå_inf,gås_inf,gått,gåtts,
        gången,gångens,gånget,gångets,
        gångna_undef,gångnas_undef,gångna_def,gångnas_def ] lex
testVerb _ _ = undefined

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
               "Swedish verbs (with present)"
               "alltenses/ParadigmsSwe.gfo"
               "mkV"
               setupVerb
               testVerb
  mainRunExperiment verbEx
