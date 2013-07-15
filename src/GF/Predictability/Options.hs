{-# LANGUAGE OverloadedStrings #-}
module GF.Predictability.Options where

import System.Log.Logger (Priority(..))
import Options.Applicative
import Shelly (FilePath(..))
import Prelude hiding (FilePath)
import Data.String (fromString)

data Options = Options
  { gfBin       :: Maybe FilePath
  , priority    :: Priority
  , htmlReport  :: Maybe FilePath
  , plotReport  :: Maybe FilePath
  } deriving (Eq,Show)

defaultOptions = Options  { gfBin = Nothing, priority = NOTICE
                          , htmlReport = Nothing, plotReport = Nothing }

parseVerbosity :: Parser Priority
parseVerbosity = flag' INFO ( long "verbose" <> help "Enable verbose mode")
        <|> flag' DEBUG ( long "debug" <> help "Enable debug mode")
        <|> pure NOTICE

parseOptions :: Parser Options
parseOptions = Options
      <$> optional (nullOption
          ( long "gf-bin"
         <> metavar "GF"
         <> help "Specify the gf binary to use instead of the one from $PATH"
         <> reader (Right . fromString)))
      <*> parseVerbosity
      <*> optional (nullOption
          ( long "html-report"
         <> metavar "HTML_FILE"
         <> help "Save a html formated report in HTML_FILE"
         <> reader (Right . fromString)))
      <*> optional (nullOption
          ( long "plot-report"
         <> metavar "PLOT_FILE"
         <> help "Save values in PLOT_FILE to be ploted by jenkins"
         <> reader (Right . fromString)))

getOptions :: IO Options
getOptions = execParser opts
  where opts = info (helper <*> parseOptions)
          ( fullDesc
         <> progDesc "Print a greeting for TARGET"
         <> header "hello - a test for optparse-applicative" )
