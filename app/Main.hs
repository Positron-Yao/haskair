{-# LANGUAGE DeriveGeneric, OverloadedStrings #-}
module Main where
import Network.Wreq
import qualified Data.ByteString.Lazy.Char8 as BL
import Control.Lens
import GHC.Generics (Generic)
import Data.Aeson -- (FromJSON, decode)
import Data.Text (Text, unpack)
import Data.Aeson.Lens -- (key, nth)
import Text.Printf

type API = String
myAPI :: API
myAPI = "3762d7ab3be74be9bb530507251506"

type City = String
localCity :: City
localCity = "wuhan"

-- 顶层结构
data WeatherCurrent = WeatherCurrent
  { location :: Location
  , current  :: Current
  } deriving (Show, Generic)

-- 位置信息
data Location = Location
  { name      :: Text
  , region    :: Text
  , country   :: Text
  , lat       :: Double
  , lon       :: Double
  , localtime :: Text  
  } deriving (Show, Generic)

-- 当前天气信息
data Current = Current
  { temp_c      :: Double
  , condition   :: Condition
  , wind_kph    :: Double
  , wind_degree :: Int
  , wind_dir    :: Text
  , pressure_mb :: Double
  , pressure_in :: Double
  , precip_mm   :: Double
  , precip_in   :: Double
  , humidity    :: Int
  , cloud       :: Int
  , feelslike_c :: Double
  , windchill_c :: Double
  , heatindex_c :: Double
  , dewpoint_c  :: Double
  , vis_km      :: Double
  , uv          :: Double
  , gust_kph    :: Double
  } deriving (Show, Generic)

-- 天气状况
data Condition = Condition
  { text :: Text
  , icon :: Text
  , code :: Int
  } deriving (Show, Generic)

instance FromJSON WeatherCurrent
instance FromJSON Location
instance FromJSON Current
instance FromJSON Condition

call_api :: API -> City -> IO (Response BL.ByteString)
call_api api city = get $ printf "http://api.weatherapi.com/v1/current.json?key=%s&q=%s&aqi=no" api city

main1 :: IO ()
main1 = do
    response <- call_api myAPI localCity
    let mwc = decode (response ^. responseBody) :: Maybe WeatherCurrent
    case mwc of
        Nothing -> putStrLn "Error: decoding failed"
        Just w -> do
            putStrLn $ show w

main :: IO ()
main = do
    response <- call_api myAPI localCity
    case decode (response ^. responseBody) :: Maybe Value of
        Nothing -> putStrLn "Error: decoding failed"
        Just w -> do
            putStr $ "City: " ++ maybe "Unknown" unpack (w ^? key "location". key "name" . _String)
            putStr $ " - " ++ maybe "Unknown" unpack (w ^? key "location". key "region" . _String)
            putStrLn $ " - " ++ maybe "Unknown" unpack (w ^? key "location". key "country" . _String)
            putStrLn $ "LocalTime: " ++ maybe "Unknown" unpack (w ^? key "location". key "localtime" . _String)
            putStrLn $ "Temperature: " ++ maybe "Unknown" show (w ^? key "current". key "temp_c" . _Number)

