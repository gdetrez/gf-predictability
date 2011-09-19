{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# OPTIONS_GHC -F -pgmF htfpp #-}
module GF.Predictability where

import GF.Predictability.GFScript
import Control.Monad.Trans.Reader (ReaderT, runReaderT, ask)
-- import Control.Monad.Trans.Writer (WriterT, runWriterT, tell)
import Control.Monad.Trans.Error (ErrorT, runErrorT)
import Control.Monad.Trans.Class (lift)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Either (rights, lefts)
import Text.Printf
import System.Log.Logger
import System (getArgs)
import System.Console.GetOpt
import Control.Monad (when)
-- ********************************* TESTING ********************************
import Test.Framework hiding (runTest, Result)
import Data.List (inits)
-- **************************************************************************

-- ********************************** DATA **********************************
-- Type alias
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

-- | The test function takes two lists of strings (first the output from GF
-- and then the entry from the lexicon) and copare them, returning True if 
-- the GF guessed right.
type TestFunction = [String] -> [String] -> Bool


-- | Every experiment starts as an instance of the Experiment data type.
-- this specifies an experiment name, the gfo file and oper function,
-- the inflected lexicon and two function: the setup function that computes
-- the succesives sequences of forms to be tested and the test function that
-- compare the output from GF with the data from the lexicon.
data Experiment = 
  Experiment
  { name :: String
  , gfoFile :: String
  , operName :: String
  , morphLexicon :: Lexicon
  , setupFunction :: SetupFunction
  , testFunction :: TestFunction
  }

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

-- | At the end of the experiment, we compute a summary of the results and
-- return a value of type Summary.
data Summary =
  Summary
   { getExperiment :: Experiment
   , getPredictability :: Double  -- ^ computed predictability
   , getDist :: Map Int Int       -- ^ a table decomposing, for each
                                  -- possible number of forms, the number of
                                  -- entry necessiting this number of forms
   , getSkipped :: Int            -- ^ the number of skipped entries
   , getSkippedDist :: Map String Int -- ^ for each skip reason, the number
                                      -- of skipped entries for this reason
   }
-- **************************************************************************

-- ******************************** LOGGING *********************************
data LogLevel = Debug
                | Warn
                | Error
                deriving (Eq, Show, Ord)

type LogMessage = (LogLevel, String)
-- **************************************************************************

-- *************************** EXPOSED FUNCTIONS ****************************
-- | Build an basic experiment. This one uses default function for setup and
-- test. (Resp.: (return . tail . inits) and (==))
mkExperiment :: String     -- ^ Experiment name
             -> String     -- ^ .gfo file
             -> String     -- ^ oper name
             -> Lexicon    -- ^ Inflected lexicon
             -> Experiment
mkExperiment n g o l = Experiment n g o l (return . tail . inits) (==)

-- | Build an experiment given the same parameters than mkExperiment and two 
-- functions in addition: setup function that take a lexicon entry and return
-- the successive lists of arguments to ge tested with the oper ; and a test
-- function that compare the output of GF to the entry from the lexicon.
mkExperimentWithFunctions :: String        -- ^ experiment name
                          -> String        -- ^ .gfo file
                          -> String        -- ^ open name
                          -> Lexicon       -- ^ inflected lexicon
                          -> SetupFunction -- ^ setup function
                          -> TestFunction  -- ^ test function
                          -> Experiment
mkExperimentWithFunctions = Experiment

-- | Run experiment and display the result.
-- This function also handle some command line options like --mk-gf-lexicon.
mainRunExperiment :: Experiment -> IO ()
mainRunExperiment = mainRunExperiments . (:[])
  
mainRunExperiments :: [Experiment] -> IO ()
mainRunExperiments experiments = do
  args <- getArgs
  let (optHelpers, nonOpt, msg) = getOpt Permute optDescr args
      options =
        (foldl (.) id optHelpers) $ Options Nothing Nothing False False
  when (testMode options) $ 
    putStrLn $ center $ "/!\\/!\\/!\\ TEST MODE /!\\/!\\/!\\"
  flip mapM_ experiments $ \e -> do
    let experiment = case (testMode options) of
          True -> e { morphLexicon = take 100 $ morphLexicon e}
          False -> e
    results <- runExperiment options experiment
    let summary = mkSummary experiment results
    pprintSummary summary
    when (testMode options) $ 
      putStrLn $ center $ "/!\\/!\\/!\\ TEST MODE /!\\/!\\/!\\"
  where center s = take (div (79 - length s) 2) (repeat ' ') ++ s

skip :: String -> Either String a
skip = Left

-- **************************************************************************

-- ***************************** OPTION PARSING *****************************
data Options = Options 
  { gfLexFile :: Maybe FilePath
  , dumpFile :: Maybe FilePath
  , debugOn :: Bool
  , testMode :: Bool
  }
-- Options declaration
optDescr :: [OptDescr (Options -> Options)]
optDescr = 
  [ Option [] ["mk-lexicon"] (ReqArg (\p o -> o {gfLexFile=Just p }) "FILE")
    "Create a GF lexicon form the data and put it in FILE"
  , Option [] ["dump-results"] (ReqArg (\p o -> o {dumpFile=Just p }) "FILE")
    "Dump results for every lexicon entries in FILE"
  , Option ['d'] ["debug"] (NoArg (\o -> o {debugOn=True})) 
    "enable debug messages"
  , Option ['t'] ["test"] (NoArg (\o -> o {testMode=True})) 
    "Test mode: use only the 100 fist entries from the lexicon (useful for debugging)"
  ]        
-- **************************************************************************

-- *************************** INTERNAL FUNCTIONS ***************************
-- | Run an experiment and return a list of results
runExperiment :: Options -> Experiment -> IO [Result]
runExperiment opts e = runExp opts e $ do
    lexicon <- getParam morphLexicon
    test <- getOption testMode
    case test of
      True -> mapM runTest $ take 100 lexicon
      False -> mapM runTest lexicon

mkSummary :: Experiment -> [Result] -> Summary
mkSummary exp l =  
  let total = length l
      valid = length $ rights $ map snd l
      skipped = total - valid
      skipDist = mkFreqDist $ lefts $ map snd l
      table = mkFreqDist $ map length $ rights $ map snd l
      predictability = avg $ map length $ rights $ map snd l
  in
   Summary exp predictability table skipped skipDist
     where mkFreqDist :: (Ord a) => [a] -> Map a Int 
           mkFreqDist = flip foldl Map.empty $ flip (Map.alter inc)
           inc Nothing = Just 1
           inc (Just n) = Just $ n + 1
           avg :: (Fractional f) => [Int] -> f
           avg l = (fromIntegral $ sum l) / (fromIntegral $ length l)
-- | Pretty printing summaries
pprintSummary :: Summary -> IO ()
pprintSummary s = do
  putStrLn $ "***** " ++ (name $ getExperiment s) ++ " *****"
  putStrLn ""
  putStrLn $ "Predictability: " ++ (show $ getPredictability s)
  putStrLn ""
  putStrLn $ "  +--------+--------+"
  putStrLn $ "  | #forms | #words |"
  putStrLn $ "  +--------+--------+"
  mapM_ (\(k,v) -> putStrLn $ printf "  | %6i | %6i |" k v) $ 
    Map.assocs $ getDist s
  putStrLn $ "  +--------+--------+"
  putStrLn ""
  putStrLn $ printf "Total: %i" (length $ morphLexicon $ getExperiment s)
  putStrLn ""
  putStrLn $ printf "Skipped: %i" (getSkipped s)
  putStrLn $ printf "Reasons:"
  mapM_ (\(k,v) -> putStrLn $ printf "\t%s (%d)" k v) $ 
    Map.assocs $ getSkippedDist s
  putStrLn ""


-- | Run one test (ie. test one entry in the lexicon.)
runTest :: LexiconEntry    -- ^ The lexicon entry to be tested
        -> Exp Result
runTest (entry,forms) = do
  debug $ "Testing entry: " ++ entry
  setup <- getParam setupFunction
  case setup forms of
    Left s -> do
      debug $ "Skipped: " ++ s
      return (entry, skip s)
    Right fs -> run fs
  where run :: [[String]] -> Exp Result
        run [] = do
          debug "\tImpossible !"
          return (entry, skip "Not found trying all possibilities")
        run (s:ss) = do
          gf_out <- mkScript s >>= runScript >>= return . lines
          debug $ "\tResult: " ++ show gf_out
          test <- getParam testFunction
          case test gf_out forms of
            True -> do
              debug "\t↳ OK"
              return (entry, return s)
            False -> run ss

-- | Create a GF script that execute the experiment's 'oper' on the given list of forms.
mkScript :: [String] -> Exp String
mkScript forms = do
  path <- getParam gfoFile
  oper <- getParam operName
  debug $ "\tTrying " ++ "cc -all " ++ oper ++ " " ++ unwords forms
  return $ unlines
    [ "i -retain " ++ path 
    , "cc -all " ++ oper ++ " " ++ unwords forms]

runScript :: String -> Exp String
runScript = lift . executeGFScript
-- **************************************************************************

-- ****************************** The Exp Monad *****************************
-- Monad definition
type Exp a = ReaderT (Options,Experiment) IO a

-- Run function
runExp :: Options -> Experiment -> Exp a -> IO a
runExp opts exp cpt = runReaderT cpt (opts, exp)

-- * Monad functions


-- Get parameter from the experiment
getParam :: (Experiment -> a) -> Exp a
getParam getP = do
  exp <- ask
  return (getP $ snd exp)

-- Get program option
getOption :: (Options -> a) -> Exp a
getOption getO = do
  exp <- ask
  return (getO $ fst exp)

-- Print debug message
debug :: String -> Exp ()
debug s = do
  on <- getOption debugOn
  when on $ lift $ putStrLn s
-- **************************************************************************

-- ********************************* TESTING ********************************
dummyLexiconEntry :: LexiconEntry
dummyLexiconEntry = ("a", ["a", "b", "c"])

dummyExperiment :: Experiment
dummyExperiment = 
  Experiment "Test experiment"
      "tests/gf/DummyParadigm.gf"               
      "mkDummy"
      [dummyLexiconEntry]
      (return . drop 1 . inits)
      (==)

dummyOptions :: Options
dummyOptions = Options Nothing Nothing True False

test_runExperiment = do
  result <- runExp dummyOptions dummyExperiment (return "ok")
  assertEqual result "ok"

test_getParam = do
  testField name
  testField gfoFile
  testField operName
  testField morphLexicon
  where testField f = do
          param <- runExp dummyOptions dummyExperiment (getParam f)
          assertEqual param (f dummyExperiment)

test_mkScript = do
  script <- runExp dummyOptions dummyExperiment (mkScript ["a", "c"])
  let correct = unlines
        [ "i -retain tests/gf/DummyParadigm.gf"
        , "cc -all mkDummy \"a\" \"c\""]
  assertEqual correct script

test_runTest = do
  result <- runExp dummyOptions dummyExperiment (runTest dummyLexiconEntry)
  assertEqual ("a", Right ["a", "b", "c"]) result