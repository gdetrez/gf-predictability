{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# OPTIONS_GHC -F -pgmF htfpp #-}
module GF.Predictability where

import GF.Predictability.GFScript
import Control.Monad.Trans.Reader (ReaderT, runReaderT, ask)
import Control.Monad.Trans.Class (lift)

--import Control.Monad.Trans.Class (lift)
-- TESTING

import Test.Framework hiding (runTest)
import Data.List (inits)

-- mainPredictabilityExperiment :: [Experiment] -> IO ()
-- mainPredictabilityExperiment ex = mapM_ doExperiment ex
--   where doExperiment = undefined

data Experiment = 
  Experiment
  { name :: String
  , gfoFile :: String
  , operName :: String
  , morphLexicon :: [[String]]
  , setupFunction :: [String] -> [[String]]
  , testFunction :: [String] -> [String] -> Bool
  }

runExperiment :: Experiment -> IO [Int]
runExperiment e = do
  runExp e $ do
    lexicon <- getParam morphLexicon
    mapM runTest lexicon


runTest :: [String]        -- ^ Input data (all forms)
        -> Exp Int  -- ^ Result: nomber of forms needed to guess corectly
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
        myReturn [] l = fail $ "Impossible to guess right (runTest)" ++ show l
        myReturn ((True,n):_) _ = return n
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


-- TESTING

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
  assertEqual 3 result