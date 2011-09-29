{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE BangPatterns #-}
{-# OPTIONS_GHC -F -pgmF htfpp #-}
module GF.Predictability ( 
  Experiment, Lexicon, TestFunction, SetupFunction,
  mkExperiment, mkExperimentWithFunctions, skip,
  mainRunExperiment, mainRunExperiments
  ) where

import Control.Monad (when)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Reader (ReaderT, runReaderT, ask)
import Data.Map (Map)
import Data.List (intercalate)
import Data.String.Utils (split)
import Data.Maybe (isJust, fromJust)
import GF.Predictability.GFScript
import System (getArgs)
import System.Console.GetOpt
import System.IO
import System.Directory (getTemporaryDirectory, removeFile)
import System.Log.Logger
import System.Log.Handler.Simple
import Text.Printf
import qualified Data.Map as Map
-- ********************************* TESTING ********************************
import Test.Framework hiding (runTest, Result)
import Data.List (inits)
-- **************************************************************************
testModeSampleSize = 1000

-- ********************************** DATA **********************************
-- | Every experiment starts as an instance of the Experiment data type.
-- this specifies an experiment name, the gfo file and oper function,
-- the inflected lexicon and two function: the setup function that computes
-- the succesives sequences of forms to be tested and the test function that
-- compare the output from GF with the data from the lexicon.
data Experiment = 
  Experiment
  { getName :: String
  , getLexicon :: Lexicon
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

-- | The test function takes two lists of strings (first the output from GF
-- and then the entry from the lexicon) and copare them, returning True if 
-- the GF guessed right.
type TestFunction = [String] -> [String] -> Bool

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

-- *************************** EXPOSED FUNCTIONS ****************************
-- | Build an basic experiment. This one uses default function for setup and
-- test. (Resp.: (return . tail . inits) and (==))
mkExperiment :: String     -- ^ Experiment name
             -> String     -- ^ .gfo file
             -> String     -- ^ oper name
             -> Lexicon    -- ^ Inflected lexicon
             -> Experiment
mkExperiment n g o l = Experiment n l g o (return . tail . inits) (==)

-- | Build an experiment given the same parameters than mkExperiment and two 
-- functions in addition: setup function that take a lexicon entry and return
-- the successive lists of arguments to ge tested with the oper ; and a test
-- function that compare the output of GF to the entry from the lexicon.
mkExperimentWithFunctions :: String        -- ^ experiment name
                          -> String        -- ^ .gfo file
                          -> String        -- ^ oper name
                          -> Lexicon       -- ^ inflected lexicon
                          -> SetupFunction -- ^ setup function
                          -> TestFunction  -- ^ test function
                          -> Experiment
mkExperimentWithFunctions name gfo oper lex sf tf =
  Experiment name lex gfo oper sf tf

-- | Run experiment and display the result.
-- This function also handle some command line options like --mk-gf-lexicon.
mainRunExperiment :: Experiment -> IO ()
mainRunExperiment = mainRunExperiments . (:[])
  
mainRunExperiments :: [Experiment] -> IO ()
mainRunExperiments experiments = do
  -- First we get the command line options
  options <- getOptions
  
  -- Then we setup the output options, using the logger module
  
  -- First shut up the default, stderr, handler
  updateGlobalLogger rootLoggerName$ setHandlers ([]::[GenericHandler Handle])
  
  -- Setting up console output according to command line options
  eh <- streamHandler stdout (debugLevel options)
  updateGlobalLogger "GF.Predictability" (addHandler eh)
  updateGlobalLogger "GF.Predictability" (setLevel DEBUG)

  -- Setting up file output
  when (isJust $ logFile options) $ do
    fh <- fileHandler (fromJust $ logFile options) NOTICE
    updateGlobalLogger "GF.Predictability" (addHandler fh)

  -- display a banner if we are in test mode
  when (testMode options) $ 
    warningM "GF.Predictability" $ center $ "/!\\/!\\ TEST MODE /!\\/!\\"
  
  let sel = experimentSelection options
  flip mapM_ experiments $ \e ->
    -- if a list of experiment names is given in the options, we only run the
    -- corresponding experiments
    when (length sel < 1 || getName e `elem` sel) $ do
      -- if in test mode, we test only 100 entry per lexicon
      let experiment = case (testMode options) of
            True -> e { getLexicon = take testModeSampleSize $ getLexicon e}
            False -> e
      noticeM "GF.Predictability" ("* " ++ (getName e))
      resultFile <- runExperiment experiment
      summary <- mkSummary resultFile
      pprintSummary summary
      removeFile resultFile
  when (testMode options) $ 
    warningM "GF.Predictability" (center $ "/!\\/!\\ TEST MODE /!\\/!\\")
  where center s = take (div (79 - length s) 2) (repeat ' ') ++ s
        getOptions = do  
          args <- getArgs
          let (optFuns, nonOpt, msg) = getOpt Permute optDescr args
          case msg of
            [] -> return $ 
             (foldl (.) id optFuns) $ Options Nothing Nothing NOTICE False []
            msgs -> fail $ unlines msgs 

skip :: String -> Either String a
skip = Left

-- **************************************************************************

-- ***************************** OPTION PARSING *****************************
data Options = Options 
  { gfLexFile :: Maybe String
  , logFile :: Maybe String
  , debugLevel :: Priority
  , testMode :: Bool
  , experimentSelection :: [String]
  }
-- Options declaration
optDescr :: [OptDescr (Options -> Options)]
optDescr = 
  [ Option [] ["mk-lexicon"] (ReqArg (\p o -> o {gfLexFile=Just p }) "FILE")
    "Create a GF lexicon form the data and put it in FILE (not implemented yet)"
  , Option ['s'] ["summary"] (ReqArg (\p o -> o { logFile=Just p }) "FILE")
    "Save experiment summary in FILE"
  , Option ['d'] ["debug"] (NoArg (\o -> o {debugLevel=DEBUG})) 
    "enable debug messages"
  , Option ['q'] ["quiet"] (NoArg (\o -> o {debugLevel=ERROR})) 
    "Disable most output"
  , Option ['t'] ["test"] (NoArg (\o -> o {testMode=True})) 
    "Test mode: use only the 100 fist entries from the lexicon (useful for debugging)"
  , Option ['e'] ["experiments"] (ReqArg (\l o -> o {experimentSelection=split "," l}) "LIST")
    "Test mode: use only the 1000 fist entries from the lexicon (useful for debugging)"
  ]
-- **************************************************************************

-- *************************** INTERNAL FUNCTIONS ***************************
-- | Run an experiment and return a list of results.
-- For performance reason, we don't want to keep all the results in
-- memory. And because of haskell lazyness mixed with the strictness of 
-- IO operation, this make it *very* difficult to aggregate the numbers along
-- the way. So to avoid this problem, we use a temporary file where
-- we dump the result. Then we can read that file to compute statistics about
-- the results.
runExperiment :: Experiment -> IO FilePath
runExperiment experiment = do
  (env,path) <- makeEnvironment experiment
  runExp env $ mapM runTest (getLexicon experiment)
  closeEnvironment env
  return path
  where makeEnvironment e = do
          -- create temp file
          tmpDir <- getTemporaryDirectory 
          (path, handle) <- openTempFile tmpDir "experiment.log"
          noticeM "GF.Predictability" $ "Temporary file: " ++ path
          return ( Env handle (getGfo e) (getOper e) (getSetup e) (getTest e)
                 , path) 
        closeEnvironment env = hClose (envHandle env)


-- | Run one test (ie. test one entry in the lexicon.)
runTest :: LexiconEntry    -- ^ The lexicon entry to be tested
        -> Exp ()
runTest (entry,forms) = do
  debug $ "➭ Testing entry: " ++ entry
  setup <- getParam envSetup
  case setup forms of
    Left s -> do
      debug $ "Skipped: " ++ s
      putResult (entry, skip s)
    Right fs -> run fs
  where run :: [[String]] -> Exp ()
        run [] = do
          debug "↳ Not found trying all possibilities\n"
          putResult (entry, skip "Not found trying all possibilities")
        run (s:ss) = do
          gfo <- getParam envGfo
          oper <- getParam envOper
          gf_out <- lift $ cc gfo oper s
          test <- getParam envTest
          case test gf_out forms of
            True -> do
              debug "✔ OK \n"
              putResult (entry, return s)
            False -> do
              debug "✖ NO \n"
              run ss

-- | Create and run a GF script that execute the experiment's 'oper'
-- on the given list of arguments using the cc command from the gf shell.
cc :: String -- ^ gfo
   -> String -- ^ oper
   -> [String] -- ^ oper's arguments
   -> IO [String] -- ^ gf output
cc gfo oper arguments = do
  let script = [ "i -retain " ++ gfo
               , "cc -all " ++ oper ++ " " ++ unwords arguments]
  debugM "GF.Predictability" $ unlines $ map ("gf> "++) script
  output <- executeGFScript (unlines script)
  debugM "GF.Predictability" output
  return (lines output)
  where unlines = intercalate "\n" -- special version of unlines that 
                                   -- doesn't add a \n at the end

-- **************************************************************************

-- ****************************** The Exp Monad *****************************
-- Monad definition
type Exp a = ReaderT ExperimentEnvironment IO a

-- | This data type is almost identical to Experiment but it has two important
-- differences: first it adds a file handle to dump the results
-- and second it doesn't have the lexicon. This is very importan otherwise the
-- lexicon cannot be garbage collected regularly
data ExperimentEnvironment = Env
  { envHandle :: Handle
  , envGfo :: String
  , envOper :: String
  , envSetup :: SetupFunction
  , envTest :: TestFunction
  }

-- Run function
runExp :: ExperimentEnvironment   -- ^ Experiment specifications & data
       -> Exp a        -- ^ Monad computation
       -> IO a
runExp env cpt = runReaderT cpt env
-- * Monad functions


-- Get parameter from the experiment
getParam :: (ExperimentEnvironment -> a) -> Exp a
getParam getP = do
  exp <- ask
  return (getP exp)

-- Print debug message
debug :: String -> Exp ()
debug s = lift $ debugM "GF.Predictability" s

-- manipulating the state
putResult :: Result -> Exp ()
putResult r = do
  -- update state
  h <- getParam envHandle
  lift $ hPutStrLn h $ show r
  -- lift $ 
    -- infoM "GF.Preditability" $ printf "%s: OK (%s %s)" w oper (unwords l)
  --lift $ infoM "GF.Predictability" $ printf "%s: Skipped (%s)" w e

-- **************************************************************************

-- ******************************** SUMMARY *********************************
-- | At the end of the experiment, we compute a summary of the results and
-- return a value of type Summary.
data Summary =
  Summary
   { getResultDist :: Map Int Int     -- ^ a table decomposing, for each
                                      -- possible number of forms, the number
                                      -- of entries necessiting this number
                                      -- of forms
   , getSkipped :: Int                -- ^ the number of skipped entries
   , getSkippedDist :: Map String Int -- ^ for each skip reason, the number
                                      -- of skipped entries for this reason
   , getTotal :: Int                  -- ^ Total number of entries
   }

emptySummary :: Summary
emptySummary = Summary Map.empty 0 Map.empty 0

-- | This function reads the data file and compute a summary of the results
mkSummary :: FilePath -> IO Summary
mkSummary path = do
  content <- readFile path
  return $ foldl aggregate emptySummary (map read $ lines content)
  where aggregate :: Summary -> Result -> Summary
        aggregate s r =   case r of
          (w, Right l) -> s { getResultDist =
                                 Map.alter incr (length l) (getResultDist s)
                            , getTotal = 1 + (getTotal s)}
          (w, Left e) -> s { getSkippedDist =
                                Map.alter incr e (getSkippedDist s)
                           , getTotal = 1 + (getTotal s)}
        incr :: Maybe Int -> Maybe Int
        incr Nothing = Just 1
        incr (Just n) = Just (n + 1)

predictability :: Summary -> Double
predictability s = 
  let a = Map.foldWithKey f 0 (getResultDist s) 
      b = fromIntegral $ Map.fold (+) 0 (getResultDist s)
  in a / b
    where f k a b = b + fromIntegral (k * a)

-- | Pretty printing summaries
pprintSummary :: Summary -> IO ()
pprintSummary s = do
  put ""
  put $ "Predictability: " ++ (show $ predictability s)
  put ""
  put $ "  +--------+--------+"
  put $ "  | #forms | #words |"
  put $ "  +--------+--------+"
  mapM_ (\(k,v) -> put $ printf "  | %6i | %6i |" k v) $ 
    Map.assocs $ getResultDist s
  put $ "  +--------+--------+"
  put ""
  put $ printf "Total: %i" (getTotal s)
  put ""
  put $ printf "Skipped: %i" (getSkipped s)
  put $ printf "Reasons:"
  mapM_ (\(k,v) -> put $ printf "\t%s (%d)" k v) $ 
    Map.assocs $ getSkippedDist s
  put ""
  where put = noticeM "GF.Predictability"
-- **************************************************************************




-- ********************************* TESTING ********************************
-- dummyLexiconEntry :: LexiconEntry
-- dummyLexiconEntry = ("a", ["a", "b", "c"])

-- dummyExperiment :: Experiment
-- dummyExperiment = 
--   Experiment "Test experiment"
--       "tests/gf/DummyParadigm.gf"               
--       "mkDummy"
--       [dummyLexiconEntry]
--       (return . drop 1 . inits)
--       (==)

-- dummyOptions :: Options
-- dummyOptions = Options Nothing Nothing True False []

-- test_runExperiment = do
--   result <- runExp dummyOptions dummyExperiment (return "ok")
--   assertEqual result "ok"

-- test_getParam = do
--   testField name
--   testField gfoFile
--   testField operName
--   testField morphLexicon
--   where testField f = do
--           param <- runExp dummyOptions dummyExperiment (getParam f)
--           assertEqual param (f dummyExperiment)

-- test_mkScript = do
--   script <- runExp dummyOptions dummyExperiment (mkScript ["a", "c"])
--   let correct = unlines
--         [ "i -retain tests/gf/DummyParadigm.gf"
--         , "cc -all mkDummy \"a\" \"c\""]
--   assertEqual correct script

-- test_runTest = do
--   result <- runExp dummyOptions dummyExperiment (runTest dummyLexiconEntry)
--   assertEqual ("a", Right ["a", "b", "c"]) result
