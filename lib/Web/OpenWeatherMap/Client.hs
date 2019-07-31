{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}

module Web.OpenWeatherMap.Client (
  getWeather,
  getBaseUrlFromCfg,
  getBaseUrlFromCfg',
  getClientEnv
) where

import Data.Text
import Data.Proxy
import Network.HTTP.Client (newManager)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Servant.API
import Servant.Client

import Config
import Web.OpenWeatherMap.Types ( WeatherData )

type WeatherAPI = "weather" :> QueryParam' '[Required] "APPID" Text 
                            :> QueryParam' '[Required] "q" Text 
                            :> QueryParam' '[Required] "units" Text 
                            :> Get '[JSON] WeatherData

askWeather :: Text -> Text -> Text -> ClientM WeatherData
askWeather = client (Proxy :: Proxy WeatherAPI)

getWeather :: ClientEnv -> Text -> Text -> IO (Either ClientError WeatherData)
getWeather env appid loc = runClientM (askWeather appid loc "metric") env 

getBaseUrlFromCfg :: Config -> BaseUrl
getBaseUrlFromCfg = 
  BaseUrl Https <$> unpack . owmHost . owmCfg 
                <*> fromIntegral . owmPort . owmCfg 
                <*> unpack . owmPath . owmCfg

getBaseUrlFromCfg' :: Config -> BaseUrl
getBaseUrlFromCfg' = do
  host <- unpack . owmHost . owmCfg
  port <- fromIntegral . owmPort . owmCfg
  path <- unpack . owmPath . owmCfg
  return (BaseUrl Https host port path)

getClientEnv :: Config -> IO ClientEnv
getClientEnv cfg = newManager tlsManagerSettings >>= \m -> return $ mkClientEnv m (getBaseUrlFromCfg cfg)

