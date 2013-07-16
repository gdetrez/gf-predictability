{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE ImplicitParams #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}
module GF.Predictability.Experiments where

import GF.Predictability.Stats
import GF.Predictability.Types
import Debug.Trace
import qualified Data.Text as T
import Data.List (isPrefixOf)
import GF.Predictability.Logging
import GF.Predictability.Options
import Shelly
import Prelude hiding (FilePath)
import Text.Printf (printf)

data Experiment = Experiment
  { title         :: String           -- ^ A title for the experiment
  , lexicon       :: FilePath         -- ^ The gf grammar where the lexicon is
  , category      :: String           -- ^ The gf category of the lexicon
  , nforms        :: Int              -- ^ The number of forms to consider.
                                      -- (It happens a lot that `l -list` and
                                      -- `cc -all` returns a different number of
                                      -- "forms" for the same lincat. This specify
                                      -- how many are relevant (starting from the first)
  , morphology    :: FilePath         -- ^ The gf file with the smartparadigms
  , smartparadigm :: Oper             -- ^ The name of the smart paradigm under test
  , setup         :: Word -> [[Text]] -- ^ Function that breaks an entry from the lexicon
                                      -- into the successive lists of arguments to
                                      -- be given to the smart paradigm,
  }
type ExperimentResult = [(String, Int)]
data ExperimentReport = ExperimentReport
  { experiment   :: String
  , entries      :: Int
  , meanCost     :: Double
  , medianCost   :: Double
  , distribution :: [Int] }
  deriving (Eq, Show)

makeReport :: Experiment -> [Int] -> ExperimentReport
makeReport e costs = ExperimentReport
  { experiment   = title e
  , entries      = length costs
  , meanCost     = mean fcosts
  , medianCost   = median fcosts
  , distribution = map count [1..maximum costs] }
  where fcosts = map fromIntegral costs
        count x = length (filter (==x) costs)

-- A few additional helpers for `ExperimentReport`s
m1, m1Percent, m2, m2Percent :: ExperimentReport -> Int
m1 = head . distribution
m1Percent er = round (fromIntegral (m1 er) / fromIntegral (entries er) * 100)
m2 = sum . take 2 . distribution
m2Percent er = round (fromIntegral (m2 er) / fromIntegral (entries er) * 100)



runExperiment :: Options -> Experiment -> IO ExperimentReport
runExperiment opts e = shelly $ silently $ do
    notice $ "*** " ++ title e ++ " ***"
    -- The first thing we do is to make sure we can find the gf binary.
    -- Otherwise we exit with an error
    gf <- findGf (gfBin opts)
    notice $ "Using gf binary: " ++ show gf
    -- Next we extract the lexicon from the ressource grammar
    lexicon <- getLexicon gf (lexicon e) (category e) (nforms e)
    notice $ show (length lexicon) ++ " entries found"
    -- if the `--limit` option has been set, we cut the lexicon
    -- to the given value
    let lexicon' = (maybe id take (limit opts)) lexicon
    -- Compute the cost of all entries in the lexicon
    costs <- mapM (wordCost gf e) lexicon'
    -- Create and return an experiment report
    return $ makeReport e costs


-- | Function that tries to find the gf binaries given the --gf-bin option
findGf :: Maybe FilePath -> Sh FilePath
findGf (Nothing) = do
  gf <- which "gf"
  case gf of
    Just p -> return p
    Nothing -> errorExit "gf is not in your path. Please specify the path to the gf binary with --gf-bin"
findGf (Just p) = do
  p' <- canonic p
  exists <- test_e p'
  unless (exists) $ errorExit "The specified gf binary does not exists"
  return p'

-- | This compute the cost of a single word
-- The cost of a word is computed as follow:
--  - first the `setup` function of the experiment is used to get all
--    possible argument sequences to the smart paradigm
--  - they are then tested it the order in which they are returned (note that
--    it is the experimenter responsability to make sure that they are returned
--    in intreasing length order)
--  - the length of the first sequence that produce the right inflection table
--    is returned
--  - if none of the sequences worked, the length of the inflection table is
--    returned
--
--  NOTE: isPrefixOf is used instead of a simple == because it happens that 
--  cc -all returns extra information (other record fields) that l -list
--  doesn't.
wordCost :: FilePath -> Experiment -> Word -> Sh Int
wordCost gf e w= do
    info $ "➭ Testing entry: " ++ show w
    outputs <- mapM (computeConcrete gf (morphology e) (smartparadigm e)) sequences
    let costs = map length sequences
    let cost = case filter ((isPrefixOf w).snd) (zip costs outputs) of
                (c,_):_ -> c
                [] -> length w
    info $ "  ↳ cost: " ++ show cost
    return cost
  where sequences = setup e w

esc :: Text -> Text
esc t = T.concat ["\"", t, "\""]

-- | Extract a lexicon from the given gfo file for the given category
getLexicon :: FilePath -> FilePath -> String -> Int -> Sh Lexicon
getLexicon gf file cat n = silently $ do
    debug $ "gf> " ++ gfcmd
    setStdin $ T.pack gfcmd
    output <- cmd gf "-run" file "+RTS" "-K32M" "-RTS"
    return $ filter (not.null) (map (take n . readLine) (T.lines output))
  where readLine line | T.null line = []
                      | otherwise    = T.splitOn ", " line
        gfcmd = "gt -cat=" ++ cat ++ " | l -list"

-- | Helper function that start a gf shell with the given gf/gfo file
-- loaded using --retain and execute the given gf function using the 
-- compute_concrete command
computeConcrete :: FilePath -> FilePath -> T.Text -> [T.Text] -> Sh Word
computeConcrete gf gfo oper args = do
    debug $ "gf> " ++ T.unpack gfcmd
    setStdin gfcmd
    output <- cmd gf "--run" "--retain" gfo
    debug $ "gf: " ++ T.unpack (T.intercalate ", " (T.lines output))
    return (T.lines output)
  where gfcmd = T.unwords ("cc":"-all":oper:args)

-- | Pretty printing reports
ppReport :: ExperimentReport -> String
ppReport er = unlines
  [ experiment er
  , printf "  mean cost: %f" (meanCost er)
  , printf "  median cost: %f" (medianCost er)
  , printf "  m=1: %d%% (%d)" (m1Percent er) (m1 er)
  , printf "  m<=2: %d%% (%d)" (m2Percent er) (m2 er)
  ]
