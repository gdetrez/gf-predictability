module GF.Predictability.Utils (
  readCSVLexicon,
  esc
  ) where

import GF.Predictability

-- | Read a csv lexicon where lines have the following form:
-- lemme, form1, form2, form3...
readCSVLexicon :: FilePath -> IO Lexicon
readCSVLexicon path = do
  content <- readFile path
  return $ map (\x -> (head x, tail x)) $ map split $ lines content
  where split :: String -> [String]
        split = split' []
        split' p [] = [reverse p]
        split' p (',':s) = reverse p:split' [] s
        split' p (c:s) = split' (c:p) s

-- | escape a string with double quotes
esc :: String -> String
esc s = "\"" ++ s ++ "\""
