{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# OPTIONS_GHC -F -pgmF htfpp #-}
module GF.Predictability where

import GF.Predictability.GFScript
import Control.Monad.Trans.Reader (ReaderT, runReaderT, ask)
import Control.Monad.Trans.Class (lift)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Either (rights, lefts)
import Text.Printf
-- ********************************* TESTING ********************************
import Test.Framework hiding (runTest, Result)
import Data.List (inits)
-- **************************************************************************

-- ********************************** DATA **********************************
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
  , morphLexicon :: [[String]]
  , setupFunction :: [String] -> [[String]]
  , testFunction :: [String] -> [String] -> Bool
  }

-- | For each entry in the lexicon, we createan intermediate Result value.
-- It contains one form from the lexicon (usualy the first one) and either
-- an error mesage or the number of forms necessay to correctly guess
-- the paradigm.
type Result = (String, Either String Int)

-- | At the end of the experiment, we compute a summary of the results and
-- return a value of type Summary.
data Summary =
  Summary
   { getExperiment :: Experiment
   , getPredictability :: Double  -- ^ computed predictability
   , getTable :: Map Int Int      -- ^ a table decomposing, for each possible 
                               -- number of forms, the number of entry
                               -- necessiting this number of forms
   , getSkipped :: Int             -- ^ the number of skipped entries
   }
-- **************************************************************************

-- *************************** EXPOSED FUNCTIONS ****************************
-- | Build an basic experiment. This one uses default function for setup and
-- test. (Resp.: (tail . inits) and (==))
mkExperiment :: String -- ^ Experiment name
             -> String -- ^ .gfo file
             -> String -- ^ oper name
             -> [[String]] -- ^ Inflected lexicon
             -> Experiment
mkExperiment n g o l = Experiment n g o l (tail . inits) (==)

-- | Run experiment and display the result.
-- This function also handle some command line options like --mk-gf-lexicon.
mainRunExperiment :: Experiment -> IO ()
mainRunExperiment e = do
  results <- runExperiment e
  let summary = mkSummary e results
  pprintSummary summary

-- **************************************************************************

-- *************************** INTERNAL FUNCTIONS ***************************
-- | Run an experiment and return a list of results
runExperiment :: Experiment -> IO [Result]
runExperiment e = do
  runExp e $ do
    lexicon <- getParam morphLexicon
    mapM runTest lexicon

mkSummary :: Experiment -> [Result] -> Summary
mkSummary exp l =  
  let total = length l
      valid = length $ rights $ map snd l
      skipped = length $ lefts $ map snd l
      table = count $ rights $ map snd l
      predictability = (fromIntegral $ sum (rights $ map snd l)) / (fromIntegral valid)
  in
   Summary exp predictability table skipped
     where count = flip foldl Map.empty $ flip (Map.alter inc)
           inc Nothing = Just 1
           inc (Just n) = Just $ n + 1
-- | Pretty printing summaries
pprintSummary :: Summary -> IO ()
pprintSummary s = do
  putStrLn $ "***** " ++ (name $ getExperiment s) ++ "*****"
  putStrLn ""
  putStrLn $ "Predictability: " ++ (show $ getPredictability s)
  putStrLn ""
  putStrLn $ "  +--------+--------+"
  putStrLn $ "  | #forms | #words |"
  putStrLn $ "  +--------+--------+"
  mapM_ (\(k,v) -> putStrLn $ printf "  | %6i | %6i |" k v) $ 
    Map.assocs $ getTable s
  putStrLn $ "  +--------+--------+"
  putStrLn ""
  putStrLn $ printf "Skipped: %i" (getSkipped s)
  putStrLn ""


-- | Run one test (ie. test one entry in the lexicon.)
runTest :: [String]        -- ^ Input data (all forms)
        -> Exp Result
runTest fs = do
  test <- getParam testFunction
  setup <- getParam setupFunction
  let inputs = setup fs
  results <- mapM testOne inputs
  myReturn results []
  where testOne forms = do
          output <- run forms
          test <- getParam testFunction
          return (test fs output, length forms)
        myReturn [] l = 
          return $ (head fs, Left $ "Impossible to guess right (runTest)" ++ show l)
        myReturn ((True,n):_) _ = return (head fs, Right n)
        myReturn (r:rs) l = myReturn rs (r:l)
        run forms = mkScript forms >>= runScript >>= return . lines
        

-- | Create a GF script that execute the experiment's 'oper' on the given list of forms.
mkScript :: [String] -> Exp String
mkScript forms = do
  path <- getParam gfoFile
  oper <- getParam operName
  return $ unlines
    [ "i -retain " ++ path 
    , "cc -all " ++ oper ++ " " ++ unwords (map (\s -> "\""++s++"\"") forms)]

runScript :: String -> Exp String
runScript = lift . executeGFScript

-- The Experiment Monad
type Exp a = ReaderT Experiment IO a

getParam :: (Experiment -> a) -> Exp a
getParam getP = do
  exp <- ask
  return (getP exp)

runExp :: Experiment -> Exp a -> IO a
runExp e = flip runReaderT e


-- ********************************* TESTING ********************************
dummyExperiment :: Experiment
dummyExperiment = 
  Experiment "Test experiment"
      "tests/gf/DummyParadigm.gf"               
      "mkDummy"
      [["a", "b", "c"]]
      (drop 1 . inits)
      (==)

test_runExperiment = do
  result <- runExp dummyExperiment (return "ok")
  assertEqual result "ok"

test_getParam = do
  testField name
  testField gfoFile
  testField operName
  testField morphLexicon
  where testField f = do
          param <- runExp dummyExperiment (getParam f)
          assertEqual param (f dummyExperiment)

test_mkScript = do
  script <- runExp dummyExperiment (mkScript ["a", "c"])
  let correct = unlines
        [ "i -retain tests/gf/DummyParadigm.gf"
        , "cc -all mkDummy \"a\" \"c\""]
  assertEqual correct script

test_runTest = do
  result <- runExp dummyExperiment (runTest ["a", "b", "c"])
  assertEqual ("a",Right 3) result