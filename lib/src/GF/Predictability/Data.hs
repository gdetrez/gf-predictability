module GF.Predictability.Data where

-- ********************************** DATA **********************************
-- | Every experiment starts as an instance of the Experiment data type.
-- this specifies an experiment name, the gfo file and oper function,
-- the inflected lexicon and two function: the setup function that computes
-- the succesives sequences of forms to be tested and the test function that
-- compare the output from GF with the data from the lexicon.
data Experiment =
  Experiment
  { getName :: String
  , getGfo :: String
  , getOper :: String
  , getSetup :: SetupFunction
  , getTest :: TestFunction
  }

-- | The setup function is the function taking an entry in the lexicon (list
-- of forms and eventually some parameters, like gender) and returning a list
-- of lists of forms that should be tested in that order.
-- For example, for english verb 'go' the returned list could be:
--  * go
--  * go, went
--  * go, went, gone
--
-- the library will test those until there one of them produces the
-- full paradigm.
--
-- The function works in a monad and should use return to return the list or
-- fail to specify that the entry should be skipped, giving the reason for
-- skipping.
type SetupFunction = [String] -> Either String [[String]]

-- | The test function is not doing the test, instead it takes the output from GF
-- (as a list of strings) and should massage it to make it comparable to the lexicon forms
-- (for example, comparable by (==))
-- 
-- This is useful because it often happens that gf returns more than forms the lexicon
type TestFunction = [String] -> [String]

-- | A lexicon is a list of lexicon entries. A lexicon entry is given by
-- an identifier to represent the entry (can be any sort of string like "œeuil_N") and the
-- list of forms and parameters (like "œuil, yeux, masculine")
type LexiconEntry = (String, [String])
type Lexicon = [LexiconEntry]

-- | For each entry in the lexicon, we create an intermediate Result value.
-- It contains the entry identifier (usually the dictionary or lematized
-- form.) and either the list of forms necessay to correctly guess the whole
-- paradigm, if successful, or a message explaining why the word has been
-- skipped.
type Result = (String, Either String [String])

-- **************************************************************************
