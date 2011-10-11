module GF.Predictability.Utils (
  readCSVLexicon,
  esc,
  mkFileName
  ) where

import GF.Predictability.Data
import Data.Char (toLower)

-- | Read a csv lexicon where lines have the following form:
-- lemme, form1, form2, form3...
readCSVLexicon :: String -> Lexicon
readCSVLexicon content =
  map (\x -> (head x, tail x)) $ map split $ lines content
  where split :: String -> [String]
        split = split' []
        split' p [] = [reverse p]
        split' p (',':s) = reverse p:split' [] s
        split' p (c:s) = split' (c:p) s

-- | escape a string with double quotes
esc :: String -> String
esc s = "\"" ++ s ++ "\""

-- | Create a non problematic file name by stripping all non ascii characters
mkFileName :: String -> String
mkFileName = map char
  where char c | c `elem` ['a'..'z'] = c
        char c | c `elem` ['A'..'Z'] = toLower c
        char ' ' = '_'
        char _ = '+'
