module GF.Predictability.GFScript where

import System.Process
import System.IO
import Control.Concurrent (forkIO)
import Control.Exception.Base (evaluate)

-- | Execute a GF script given as an haskell string.
-- Return an other string containing the output from GF.
-- GF is called using the option '-run' so the output should be
-- clean.
executeGFScript :: String -> IO String
executeGFScript script = do
  readProcess "gf" ["-run"] script
  
  
  
  
  
  
  -- --putStrLn "Starting GF"
  -- (inp,out,_,pid) <- runInteractiveCommand "gf -run"
  -- hSetBinaryMode inp False
  -- --putStrLn "Forking"
  -- forkIO $ do
  --   hPutStr inp script
  --   hClose inp
  -- --putStrLn "Reading GF output"
  -- output <- hGetContents out
  
  -- -- Using evaluate is a trick to force haskell to
  -- -- read the output buffer entierly.
  -- -- Otherwise, if the string is read lazily, the program can finish
  -- -- before we have actually read anything.
  -- evaluate $ length output
  
  -- --hPutStrLn inp script 
  -- -- putStrLn "Waiting for GF process to terminate"
  -- waitForProcess pid
  
  -- --errors <- hGetContents err
  -- --evaluate $ length errors

  -- hClose out
  -- --hClose err
  -- --s <- waitForProcess pid
  -- return output
