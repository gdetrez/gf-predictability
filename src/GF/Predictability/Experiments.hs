{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE ImplicitParams #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}
module GF.Predictability.Experiments where

import GF.Predictability.Stats
import GF.Predictability.Types
import Debug.Trace
import qualified Data.Text.Lazy as LT
import Data.List (isPrefixOf)
import GF.Predictability.Logging
import GF.Predictability.Options
import Shelly
import Prelude hiding (FilePath)

data Experiment = Experiment
  { title         :: String
  , lexicon       :: FilePath
  , category      :: String
  , morphology    :: FilePath
  , smartparadigm :: Oper
  , setup         :: Word -> [[Text]] }

type ExperimentResult = [(String, Int)]
data ExperimentReport = ExperimentReport
  { experiment   :: String
  , entries      :: Int
  , meanCost     :: Double
  , medianCost   :: Double
  , m1           :: Int
  , m2           :: Int
  , distribution :: [Int] }
  deriving (Eq, Show)

makeReport :: Experiment -> [Int] -> ExperimentReport
makeReport e costs = ExperimentReport
  { experiment   = title e
  , entries      = length costs
  , meanCost     = mean fcosts
  , medianCost   = median fcosts
  , m1           = count 1
  , m2           = length (filter (<=2) costs)
  , distribution = map count [1..maximum costs] }
  where fcosts = map fromIntegral costs
        count x = length (filter (==x) costs)

runExperiment :: Options -> Experiment -> IO ExperimentReport
runExperiment opts e = shelly $ silently $ do
    notice $ "*** " ++ title e ++ " ***"
    -- The first thing we do is to make sure we can find the gf binary.
    -- Otherwise we exit with an error
    gf <- findGf (gfBin opts)
    notice $ "Using gf binary: " ++ show gf
    -- Next we extract the lexicon from the ressource grammar
    lexicon <- getLexicon gf (lexicon e) (category e)
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
esc t = LT.concat ["\"", t, "\""]

-- | Extract a lexicon from the given gfo file for the given category
getLexicon :: FilePath -> FilePath -> String -> Sh Lexicon
getLexicon gf file cat = silently $ do
    debug $ "gf> " ++ gfcmd
    setStdin $ LT.pack gfcmd
    output <- cmd gf "-run" file "+RTS" "-K32M" "-RTS"
    return $ filter (not.null) (map readLine (LT.lines output))
  where readLine line | LT.null line = []
                      | otherwise    = LT.splitOn ", " line
        gfcmd = "gt -cat=" ++ cat ++ " | l -list"

-- | Helper function that start a gf shell with the given gf/gfo file
-- loaded using --retain and execute the given gf function using the 
-- compute_concrete command
computeConcrete :: FilePath -> FilePath -> LT.Text -> [LT.Text] -> Sh Word
computeConcrete gf gfo oper args = silently $ do
    debug $ "gf> " ++ LT.unpack gfcmd
    setStdin gfcmd
    output <- cmd gf "--run" "--retain" gfo
    debug $ "gf: " ++ LT.unpack (LT.intercalate ", " (LT.lines output))
    return (LT.lines output)
  where gfcmd = LT.unwords ("cc":"-all":oper:args)
