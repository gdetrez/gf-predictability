{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# OPTIONS_GHC -F -pgmF htfpp #-}
module GF.Predictability where

import GF.Predictability.GFScript
import Control.Monad.Trans.Reader (ReaderT, runReaderT, ask)
import Control.Monad.Trans.State (StateT, execStateT, get, put)
import Control.Monad.Trans.Error (ErrorT, runErrorT)
import Control.Monad.Trans.Class (lift)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Either (rights, lefts)
import Text.Printf
import System.Log.Logger
import System (getArgs)
import System.IO (Handle, openFile, IOMode(..), hPutStrLn, hClose)
import System.Console.GetOpt
import Control.Monad (when)
import Data.String.Utils (split)
import Debug.Trace (trace)
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

-- | To avoid consumming too much memory, we don't keep all the result
-- but aggregate them in a value of the following type:
data ResultAggregate = ResultAggregate
   { resultDist :: Map Int Int
   , errorDist :: Map String Int
   }
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

-- | The environment represent the global settings in which an experiment is 
-- ran.
data Environment = 
  Environment
  { envGfLexFileHandle :: Maybe Handle
  , envDumpFileHandle :: Maybe Handle
  , envOptions :: Options
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
  -- First we get the options from command line
  options <- getOptions
  -- display a banner in test mode
  when (testMode options) $ 
    putStrLn $ center $ "/!\\/!\\/!\\ TEST MODE /!\\/!\\/!\\"
  
  environment <- openEnvironment options
  -- if a list of experiment names is given in the options, we only run the
  -- corresponding experiments
  let sel = experimentSelection options
  flip mapM_ experiments $ \e -> when (length sel < 1 || name e `elem` sel)$do
    -- in test mode, we test only 100 entry per lexicon
    let experiment = case (testMode options) of
          True -> e { morphLexicon = take 100 $ morphLexicon e}
          False -> e
    summary <- runExperiment environment experiment
    pprintSummary summary
  closeEnvironment environment
  when (testMode options) $ 
    putStrLn $ center $ "/!\\/!\\/!\\ TEST MODE /!\\/!\\/!\\"
  where center s = take (div (79 - length s) 2) (repeat ' ') ++ s
        getOptions = do  
          args <- getArgs
          let (optFuns, nonOpt, msg) = getOpt Permute optDescr args
          case msg of
            [] -> return $ 
             (foldl (.) id optFuns) $ Options Nothing Nothing False False []
            msgs -> fail $ unlines msgs 
        openEnvironment :: Options -> IO Environment
        openEnvironment options = do
          hDump <- case (dumpFile options) of
            Nothing -> return Nothing
            Just path -> openFile path WriteMode >>= return . Just
          hGFL <- case (gfLexFile options) of
            Nothing -> return Nothing
            Just path -> openFile path WriteMode >>= return . Just
          return $ Environment hGFL hDump options
        closeEnvironment env = do
          case envDumpFileHandle env of
            Nothing -> return ()
            Just h -> hClose h
          case envGfLexFileHandle env of
            Nothing -> return ()
            Just h -> hClose h
            
skip :: String -> Either String a
skip = Left

-- **************************************************************************

-- ***************************** OPTION PARSING *****************************
data Options = Options 
  { gfLexFile :: Maybe String
  , dumpFile :: Maybe String
  , debugOn :: Bool
  , testMode :: Bool
  , experimentSelection :: [String]
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
  , Option ['e'] ["experiments"] (ReqArg (\l o -> o {experimentSelection=split "," l}) "LIST")
    "Test mode: use only the 100 fist entries from the lexicon (useful for debugging)"
  ]        
-- **************************************************************************

-- *************************** INTERNAL FUNCTIONS ***************************
-- | Run an experiment and return a list of results
runExperiment :: Environment -> Experiment -> IO Summary
runExperiment environment experiment = runExp environment experiment $ do
    lexicon <- getParam morphLexicon
    test <- getOption testMode
    case test of
      True -> mapM runTest $ take 100 lexicon
      False -> mapM runTest lexicon

mkSummary :: Experiment -> ResultAggregate -> Summary
mkSummary exp agg =
  let skipDist = errorDist agg
      validDist = resultDist agg
      valid = Map.fold (+) 0 validDist
      skipped = Map.fold (+) 0 skipDist
      total = valid + skipped
      predictability = (Map.foldWithKey f 0 validDist) / fromIntegral valid
        where f k a b = b + fromIntegral (k * a)
  in
   Summary exp predictability validDist skipped skipDist

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
        -> Exp ()
runTest (entry,forms) = do
  debug $ "Testing entry: " ++ entry
  setup <- getParam setupFunction
  case setup forms of
    Left s -> do
      debug $ "Skipped: " ++ s
      putResult (entry, skip s)
    Right fs -> run fs
  where run :: [[String]] -> Exp ()
        run [] = do
          debug "\tImpossible !"
          putResult (entry, skip "Not found trying all possibilities")
        run (s:ss) = do
          gf_out <- mkScript s >>= runScript >>= return . lines
          debug $ "\tResult: " ++ show gf_out
          test <- getParam testFunction
          case test gf_out forms of
            True -> do
              debug "\t↳ OK"
              putResult (entry, return s)
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
runScript = lift . lift . executeGFScript
-- **************************************************************************

-- ****************************** The Exp Monad *****************************
-- Monad definition
type Exp a = ReaderT (Environment,Experiment) (StateT ResultAggregate IO) a

-- Run function
runExp :: Environment
       -> Experiment   -- ^ Experiment specifications & data
       -> Exp a        -- ^ Monad computation
       -> IO Summary
runExp env exp cpt = do
  aggregate <- execStateT (runReaderT cpt (env, exp)) 
               (ResultAggregate Map.empty Map.empty)
  return $ mkSummary exp aggregate
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
  return (getO $ envOptions $ fst exp)

-- Print debug message
debug :: String -> Exp ()
debug s = do
  on <- getOption debugOn
  when on $ lift $ lift $ putStrLn s

-- manipulating the state
putResult :: Result -> Exp ()
putResult r = do
  -- update state
  state <- lift get
  case r of
    (_, Right l) ->
      lift $ put $ state { resultDist = Map.alter incr (length l) (resultDist state)}
    (_, Left e) -> 
      lift $ put $ state { errorDist = Map.alter incr e (errorDist state)}
  -- write in dump file
  env <- ask >>= return . fst
  case (envDumpFileHandle env, r) of
    (Nothing,_) -> return ()
    (Just h, (w, Right fs)) -> do
      oper <- getParam operName
      lift $ lift $ hPutStrLn h $ printf "%s: OK (%s %s)" w oper (unwords fs)
    (Just h, (w, Left e)) ->
      lift $ lift $ hPutStrLn h $ printf "%s: Skipped (%s)" w e
  where incr :: Maybe Int -> Maybe Int
        incr Nothing = Just 1
        incr (Just n) = Just (n + 1)
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
