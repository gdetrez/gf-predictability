main = interact (unlines . map mkOne . lines)

mkOne l = case words l of
  w:ws -> concat (w:w:ws)  -- duplicate first word, remove spaces
  _ -> l

