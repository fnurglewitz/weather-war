{-# LANGUAGE DeriveGeneric #-}

module Config (
  Config (..),
  ServerConfig (..),
  OpenWeatherMapCfg (..),
  loadCfg
) where

import Data.Text
import Dhall

data Config = Config {
  srvCfg :: ServerConfig,
  owmCfg :: OpenWeatherMapCfg,
  locations :: [Text]
} deriving (Generic, Show)

data ServerConfig = ServerConfig {
  srvPort :: Integer
} deriving (Generic, Show)

data OpenWeatherMapCfg = OpenWeatherMapCfg {
  owmHost :: Text,
  owmPort :: Integer,
  owmPath :: Text,
  owmAppId :: Text
} deriving (Generic, Show)

instance Interpret OpenWeatherMapCfg
instance Interpret ServerConfig
instance Interpret Config

loadCfg :: Text -> IO Config
loadCfg = input auto
