module Stats where

import Data.List

-- |Numerically stable mean
mean :: [Double] -> Double
mean x = sum x / (fromIntegral (length x))

-- |Median
median :: [Double] -> Double
median x | odd n  = head  $ drop (n `div` 2) x'
         | even n = mean $ take 2 $ drop i x'
  where i = (length x' `div` 2) - 1
        x' = sort x
        n  = length x

