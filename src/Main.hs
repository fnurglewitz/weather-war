{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Concurrent.STM.TVar
import Control.Concurrent (forkIO, threadDelay)
import Control.Monad ((>=>), forever)
import Data.Either
import Data.List
import Data.Ord
import Data.Text
import GHC.Conc.Sync (atomically)
import System.Environment
import Text.Printf

import Config
import WeatherWar
import Server
import qualified Web.OpenWeatherMap.Client as C
import qualified Web.OpenWeatherMap.Types as T

main :: IO ()
main = do
  argv <- getArgs
  cfg <- (loadCfg . pack . cfgPathFromArgv) argv
  env <- C.getClientEnv cfg
  tv <- newTVarIO [] :: IO (TVar [HighScore])
  forkIO (runServer (srvCfg cfg) tv)
  forever $ do
    ws <- mapM (C.getWeather env (owmAppId . owmCfg $ cfg)) (locations cfg)
    let results = sort $ toHighScore <$> rights ws
    -- let results = [HS ("Valle di Maddaloni",22.60712),HS ("Pesaro",22.232445000000002),HS ("Pescara",22.214840000000002),HS ("Reggio Calabria",22.156285),HS ("Udine",22.0426),HS ("Bari",21.7774),HS ("Bolzano",21.7525),HS ("Piacenza",21.58883),HS ("Imperia",21.416594999999997),HS ("Triest",21.402260000000002),HS ("Genoa",21.253425),HS ("Modena",21.20736),HS ("Bologna",20.57288),HS ("Rome",20.23108),HS ("Turin",20.183754999999998),HS ("L'Aquila",20.16938),HS ("Pianoro",20.0208),HS ("Florence",19.325329999999997),HS ("Pisa",19.28),HS ("Aosta",16.61768),HS ("Cagliari",16.40377),HS ("Oslo",12.30396)] :: [HighScore]
    saveHS tv results
    threadDelay 300000000
  where
    cfgPathFromArgv :: [String] -> String
    cfgPathFromArgv [] = "./config"
    cfgPathFromArgv (x:_) = x

    saveHS :: TVar [HighScore] -> [HighScore] -> IO ()
    saveHS tv hs = atomically $ do
      m <- readTVar tv
      writeTVar tv hs

-- roundToStr :: (PrintfArg a, Floating a) => Int -> a -> Text
-- roundToStr n a = pack $ printf "%0.*f" n a 
