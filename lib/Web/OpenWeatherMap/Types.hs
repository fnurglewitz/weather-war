{-# LANGUAGE OverloadedStrings #-}

module Web.OpenWeatherMap.Types where

import Control.Applicative (optional)
import Data.Aeson
import Data.Text

data Coordinates = Coordinates {
  longitude :: Double,
  latitude :: Double
} deriving (Show,Eq)

data Weather = Weather {
  weatherId :: Int,
  weatherMain :: Text,
  weatherDescription :: Text,
  weatherIcon :: Text
} deriving (Show,Eq)

data Main = Main {
  temp :: Double,
  pressure :: Int,
  humidity :: Int,
  tempMin :: Double,
  tempMax :: Double,
  seaLevel :: Maybe Int,
  grndLevel :: Maybe Int
} deriving (Show,Eq)

data Wind = Wind {
  windSpeed :: Double,
  windDeg :: Int
} deriving (Show,Eq)

data Rain = Rain {
  rainOneHour :: Int,
  rainThreeHours :: Int
} deriving (Show,Eq)

data Snow = Snow {
  snowOneHour :: Int,
  snowThreeHours :: Int
} deriving (Show,Eq)

newtype Clouds = Clouds {
  cloudsAll :: Int
} deriving (Show,Eq)

data Sys = Sys {
  sysType :: Int,
  sysId :: Int,
  sysMessage :: Double,
  sysCountry :: Text,
  sysSunrise :: Int,
  sysSunset :: Int
} deriving (Show,Eq)

data WeatherData = WeatherData {
  coord :: Coordinates,
  weather :: [Weather],
  base :: Text,
  main :: Main,
  wind :: Wind,
  clouds :: Clouds,
  rain :: Maybe Rain,
  snow :: Maybe Snow,
  dt :: Int,
  sys :: Sys,
  timezone :: Int,
  id :: Int,
  cityName :: Text,
  cod :: Int
} deriving (Show,Eq)

instance FromJSON Coordinates where
  parseJSON (Object o) =
    Coordinates <$> o .: "lon"
                <*> o .: "lat"

instance FromJSON Weather where
  parseJSON (Object o) =
   Weather <$> o .: "id"
           <*> o .: "main"
           <*> o .: "description"
           <*> o .: "icon"

instance FromJSON Main where
  parseJSON (Object o) =
    Main <$> o .: "temp"
         <*> o .: "pressure"
         <*> o .: "humidity"
         <*> o .: "temp_min"
         <*> o .: "temp_max"
         <*> optional (o .: "sea_level")
         <*> optional (o .: "grnd_level")

instance FromJSON Wind where
  parseJSON (Object o) =
    Wind <$> o .: "speed"
         <*> o .:? "deg" .!= 0

instance FromJSON Rain where
  parseJSON (Object o) =
    Rain <$> o .: "1h"
         <*> o .: "3h"

instance FromJSON Snow where
  parseJSON (Object o) =
    Snow <$> o .: "1h"
         <*> o .: "3h"

instance FromJSON Clouds where
  parseJSON (Object o) =
    Clouds <$> o .: "all"

instance FromJSON Sys where
  parseJSON  (Object o) =
    Sys <$> o .: "type"
        <*> o .: "id"
        <*> o .: "message"
        <*> o .: "country"
        <*> o .: "sunrise"
        <*> o .: "sunset"

instance FromJSON WeatherData where
  parseJSON (Object o) =
    WeatherData <$> o .: "coord"
                <*> o .: "weather"
                <*> o .: "base"
                <*> o .: "main"
                <*> o .: "wind"
                <*> o .: "clouds"
                <*> optional (o .: "rain")
                <*> optional (o .: "snow")
                <*> o .: "dt"
                <*> o .: "sys"
                <*> o .: "timezone"
                <*> o .: "id"
                <*> o .: "name"
                <*> o .: "cod"

