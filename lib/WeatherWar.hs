module WeatherWar (
  weatherScore,
  toHighScore,
  HighScore (..)
) where

import Data.Text

import Web.OpenWeatherMap.Types

newtype HighScore = HS (Text, Double) deriving (Eq, Show)

weatherScore :: WeatherData -> Double
weatherScore wd =
  let (Main t _ h _ _ _ _) = main wd 
   in discomfortIndex t h

discomfortIndex :: Double -> Int -> Double
discomfortIndex temp humidity = 
  let hpc = fromIntegral humidity/100
   in temp - 0.55 * (1-hpc) * (temp - 14.5)

windChillIndex :: Double -> Double -> Double
windChillIndex temp speed =
  let kmh = speed * 3.6
      v16 = kmh ** 0.16
   in 13.12 + (0.6215 * temp) - (11.37 * v16) + (0.3965 * temp * v16)

toHighScore :: WeatherData -> HighScore
toHighScore = HS . ((,) <$> cityName <*> weatherScore)

instance Ord HighScore where
  compare (HS (_,s)) (HS (_,s')) = compare s' s
