module GF.Predictability.Logging where

import System.Log.Logger
import GF.Predictability.Options
import Shelly

loggerName :: String
loggerName = ""

setLogger :: Options -> IO ()
setLogger options = updateGlobalLogger loggerName (setLevel (priority options))

debug :: String -> Sh ()
debug = liftIO . debugM loggerName

info :: String -> Sh ()
info = liftIO . infoM loggerName

notice :: String -> Sh ()
notice = liftIO . noticeM loggerName
