module GF.Predictability.Types (Word, Lexicon, Oper, Text) where

import Data.Text.Lazy (Text)

-- | As defined in the paper, a word is a element of the lexicon,
-- that is, an inflection table of size n
type Word = [Text]

-- | A lexicon is a ﬁnite set of inflection tables, i.e. a list of words
type Lexicon = [Word]

-- | A gf operator
type Oper = Text
