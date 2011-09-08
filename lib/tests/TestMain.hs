{-# OPTIONS_GHC -F -pgmF htfpp #-}
module Main where

import System.Environment ( getArgs )
import Test.Framework
import qualified GFScriptTests
import qualified GF.Predictability

main = do args <- getArgs
          runTestWithArgs args 
            [ GFScriptTests.allHTFTests
            , GF.Predictability.allHTFTests
            ]
                    
