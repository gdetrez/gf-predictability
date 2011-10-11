{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE BangPatterns #-}
{-# OPTIONS_GHC -F -pgmF htfpp #-}
module GF.Predictability (
  Experiment, Lexicon, TestFunction, SetupFunction,
  mkExperiment, mkExperimentWithFunctions, skip,
  mainRunExperiment
  ) where

import Control.Monad (when)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Reader (ReaderT, runReaderT, ask)
import Data.Map (Map)
import Data.List (intercalate, foldl', inits)
import Data.String.Utils (split)
import Data.Maybe (isJust, fromJust)
import System (getArgs)
import System.Console.GetOpt
import System.Path (splitExt)
import System.IO
import System.Directory (getTemporaryDirectory, removeFile)
import System.Log.Logger
import System.Log.Handler.Simple
import Text.Printf
import qualified Data.Map as Map

import GF.Predictability.GFScript
import GF.Predictability.Data
import GF.Predictability.Utils
-- * TESTING *
import Test.Framework hiding (runTest, Result)
--
-- **

-- *************************** EXPOSED FUNCTIONS ****************************
-- | Build an basic experiment. This one uses default function for setup and
-- test. (Resp.: (return . tail . inits) and (==))
mkExperiment :: String     -- ^ Experiment name
             -> String     -- ^ .gfo file
             -> String     -- ^ oper name
             -> Experiment
mkExperiment n g o = Experiment n g o (return . tail . inits) (==)

-- | Build an experiment given the same parameters than mkExperiment and two
-- functions in addition: setup function that take a lexicon entry and return
-- the successive lists of arguments to ge tested with the oper ; and a test
-- function that compare the output of GF to the entry from the lexicon.
mkExperimentWithFunctions :: String        -- ^ experiment name
                          -> String        -- ^ .gfo file
                          -> String        -- ^ oper name
                          -> SetupFunction -- ^ setup function
                          -> TestFunction  -- ^ test function
                          -> Experiment
mkExperimentWithFunctions name gfo oper sf tf =
  Experiment name gfo oper sf tf

-- | Run experiment and display the result.
-- This function also handle command line options.
mainRunExperiment :: Experiment -> IO ()
mainRunExperiment experiment = do
  -- First we get the command line options
  options <- getOptions
  
  -- Then we setup the output options, using the logger module
  -- First shut up the default, stderr, handler
  updateGlobalLogger rootLoggerName$ setHandlers ([]::[GenericHandler Handle])
  -- Setting up console output according to command line options
  eh <- streamHandler stdout (debugLevel options)
  updateGlobalLogger "GF.Predictability" (addHandler eh)
  updateGlobalLogger "GF.Predictability" (setLevel DEBUG)
  -- Setting up summary file output if required by the options
  when (isJust $ summaryFile options) $ do
    fh <- fileHandler (fromJust $ summaryFile options) NOTICE
    updateGlobalLogger "GF.Predictability" (addHandler fh)
  -- Setting up result file output
  let resultFile = case getResultFile options of
        Nothing -> mkFileName (getName experiment) ++ ".results"
        Just f -> f
  
  lexicon <- (return . readCSVLexicon) =<< case getLexiconFile options of
    Nothing -> getContents
    Just f -> readFile f
  
  noticeM "GF.Predictability" ("Experiment: " ++ (getName experiment))
  runExperiment experiment lexicon resultFile
  summary <- mkSummary resultFile
  pprintSummary summary
  where center s = take (div (79 - length s) 2) (repeat ' ') ++ s
-- **************************************************************************

-- ***************************** OPTION PARSING *****************************
data Options = Options
  { getLexiconFile :: Maybe String
  , getResultFile :: Maybe String
  , summaryFile :: Maybe String
  , debugLevel :: Priority
  }
-- Options declaration
optDescr :: [OptDescr (Options -> Options)]
optDescr =
  [ Option [] ["lexicon"] (ReqArg (\p o -> o { getLexiconFile=Just p }) "FILE")
    "Read lexicon from FILE. Read from stdin if not specified"
  , Option [] ["summary"] (ReqArg (\p o -> o { summaryFile=Just p }) "FILE")
    "Save experiment summary in FILE"
  , Option [] ["results"] (ReqArg (\p o -> o { getResultFile=Just p }) "FILE")
    "Save experiment results in FILE"
  , Option ['d'] ["debug"] (NoArg (\o -> o {debugLevel=DEBUG}))
    "enable debug messages"
  , Option ['q'] ["quiet"] (NoArg (\o -> o {debugLevel=ERROR}))
    "Disable most output"
  ]

getOptions :: IO Options
getOptions = do
  args <- getArgs
  let (optFuns, nonOpt, msg) = getOpt Permute optDescr args
  case msg of
    [] -> return $ foldl (.) id optFuns defaultOptions
    msgs -> fail $ unlines msgs
  where defaultOptions = Options Nothing Nothing Nothing NOTICE
-- **************************************************************************

-- *************************** INTERNAL FUNCTIONS ***************************
-- | Run an experiment and return a list of results.
-- For performance reason, we don't want to keep all the results in
-- memory. And because of haskell lazyness mixed with the strictness of
-- IO operation, this make it *very* difficult to aggregate the numbers along
-- the way. So to avoid this problem, we use a temporary file where
-- we dump the result. Then we can read that file to compute statistics about
-- the results.
runExperiment :: Experiment ->Lexicon ->  FilePath -> IO ()
runExperiment experiment lexicon logFile = do
  env <- makeEnvironment experiment logFile
  runExp env $ mapM runTest lexicon
  closeEnvironment env
  where makeEnvironment e f = do
          h <- openFile f WriteMode
          noticeM "GF.Predictability" $ "Log file: " ++ f
          return $ ( Env h (getGfo e) (getOper e) (getSetup e) (getTest e))
        closeEnvironment env = hClose (envHandle env)


-- | Run one test (ie. test one entry in the lexicon.)
runTest :: LexiconEntry    -- ^ The lexicon entry to be tested
        -> Exp ()
runTest (entry,forms) = do
  debug $ "➭ Testing entry: " ++ entry
  debug $ show forms
  setup <- getParam envSetup
  case setup forms of
    Left s -> do
      debug $ "Skipped: " ++ s
      putResult (entry, skip s)
    Right fs -> run fs
  where run :: [[String]] -> Exp ()
        run [] = do
          debug "↳ Not found trying all possibilities\n"
          --putResult (entry, return forms)
          putResult (entry, skip "Not found")
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

-- Shortcut for error reporting
skip :: String -> Either String a
skip = Left


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
  return $ foldl' aggregate emptySummary (map read $ lines content)
  where aggregate :: Summary -> Result -> Summary
        aggregate !s r =   case r of
          (w, Right l) ->
            let newResultDist = incr_map (getResultDist s) (length l)
                newTotal = 1 + (getTotal s)
            in newResultDist `seq` newTotal `seq` s { getResultDist = newResultDist, getTotal = newTotal }
          (w, Left e) ->
            let newSkippedDist = incr_map (getSkippedDist s) e
                newTotal = 1 + (getTotal s)
                newSkipped = 1 + (getSkipped s)
            in newSkippedDist `seq` newTotal `seq`newSkipped `seq` s { getSkippedDist = newSkippedDist, getTotal = newTotal, getSkipped = newSkipped }
        incr :: Maybe Int -> Maybe Int
        incr Nothing = Just 1
        incr (Just !n) = Just (n + 1)
        incr_map :: (Ord k) => Map k Int -> k -> Map k Int
        incr_map m k =
          case Map.lookup k m of
            Nothing -> Map.insert k 1 m
            Just n -> let n' = n + 1 in n' `seq` Map.insert k n' m

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
  put $ "  +--------+--------+"
  put $ "  | #forms | #words |"
  put $ "  +--------+--------+"
  mapM_ (\(k,v) -> put $ printf "  | %6i | %6i |" k v) $
    Map.assocs $ getResultDist s
  put $ "  +--------+--------+"
  put ""
  put $ printf "Total: %i" (Map.fold (+) 0 (getResultDist s))
  put ""
  put $ "Predictability: " ++ (show $ predictability s)
  put ""
  put $ printf "Skipped: %i" (getSkipped s)
  put $ printf "Reasons:"
  mapM_ (\(k,v) -> put $ printf "\t%s (%d)" k v) $
    Map.assocs $ getSkippedDist s
  put ""
  put $ printf "Total (incl. skipped): %i" (getTotal s)
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
