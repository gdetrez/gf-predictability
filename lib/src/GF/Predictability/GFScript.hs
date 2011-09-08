module GF.Predictability.GFScript where

import System.Process
import System.IO
import Control.Concurrent (forkIO)

-- | Execute a GF script given as an haskell string.
-- Return an other string containing the output from GF.
-- GF is called using the option '-run' so the output should be
-- clean.
executeGFScript :: String -> IO String
executeGFScript script = do
  (inp,out,err,pid) <- runInteractiveCommand "gf -run"
  hSetBinaryMode inp False
  -- forkIO (hPutStr inp script)
  hPutStrLn inp script 
  hClose inp
  out <- hGetContents out
  -- err <- pipeGetContents errC
  s <- waitForProcess pid
  return out
