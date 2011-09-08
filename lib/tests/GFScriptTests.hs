{- -*- coding: utf-8 -*- -}
{-# OPTIONS_GHC -F -pgmF htfpp #-}
module GFScriptTests where

import Test.Framework
import GF.Predictability.GFScript

-- simple test to figure out if the GF wraper is working
test_simple_script = do
  let script = unlines
       [ "! echo \"test\"" ]
  output <- executeGFScript script
  assertEqual output "test\n"
  
-- test a simple cc
test_cc_script = do
  let script = unlines
        [ "i -retain tests/gf/DummyParadigm.gf" 
        , "cc -all mkDummy \"a\" \"b\" \"c\""]
  output <- executeGFScript script
  assertEqual output "a\nb\nc\n"

