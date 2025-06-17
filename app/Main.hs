{-# LANGUAGE DeriveGeneric, OverloadedStrings #-}
module Main where
import qualified Network.Wreq as W
import qualified Data.ByteString.Lazy.Char8 as BL
import Control.Lens
import Data.Aeson -- (FromJSON, decode)
import Data.Aeson.Lens -- (key, nth)
import Data.Text (Text, pack, unpack)
import Text.Printf (printf)
import Text.Toml (parseTomlDoc)
import qualified Options.Applicative as Opt
import Options.Applicative
import System.Directory (getHomeDirectory, doesFileExist)
import System.FilePath ((</>))
import System.Exit
-- import Debug.Trace

type API_Key = String
type City = String
type Lang = String

data Config = Config {
    city :: City,
    lang :: Lang
} deriving (Show)

configParser :: Parser Config
configParser = Config
  <$> Opt.argument str
      ( metavar "CITY"     -- 帮助中的占位符
      <> value ""
      <> help "City name"   -- 帮助文本
      )
  <*> strOption
      ( long "lang"
      <> short 'l'
      <> metavar "LANG"
      <> value ""
      <> help "Language code (default: zh)"
      )

opts :: ParserInfo Config
opts = info (configParser <**> helper)
  ( fullDesc
  <> progDesc "Get weather information from weatherapi.com"
  <> header "haskair - Haskell weather app" )

readToml :: IO (Either String String)
readToml = do
    homeDir <- getHomeDirectory
    let tomlFile = homeDir </> ".config" </> "haskair.toml"
    exists <- doesFileExist tomlFile
    if exists
        then Right <$> readFile tomlFile
        else return $ Left "No config file found"

call_api :: API_Key -> City -> IO (W.Response BL.ByteString)
call_api a c = W.get $ printf "http://api.weatherapi.com/v1/forecast.json?key=%s&q=%s&days=1&aqi=no&alert=no" a c

-- [TODO] 待适配中文字符
fmt :: String -> String
fmt s = printf "%15s" s

maybeS :: Maybe Text -> String
maybeS s = maybe "" unpack s

maybeN :: (Num a, Show a) => Maybe a -> String
maybeN n = maybe "" show n

main :: IO ()
main = do
    config <- execParser opts
    tomldata <- readToml
    case tomldata of
        Left e -> putStrLn e >> exitFailure
        Right tomltext -> do
            let eitherToml = parseTomlDoc "" (pack tomltext)
            case eitherToml of
                Left e -> print e >> exitFailure
                Right toml -> do
                    let jtoml = toJSON toml
                    -- trace "jtoml: " (print jtoml)
                    response <- let a = (maybe "" unpack (jtoml ^? key "api_key" . _String))
                                    c = if null (city config) then (maybe "" unpack (jtoml ^? key "city" . _String)) else city config
                        in call_api a c
                   -- 语言
                    case if null (lang config) then (maybeS (jtoml ^? key "default_lang" . _String)) else (lang config) of
                        "zh" -> do
                            let Just trans = (jtoml ^? key "lang". key "zh")
                            case decode (response ^. W.responseBody) :: Maybe Value of
                                Nothing -> putStrLn (maybeS (trans ^? key "decode_error". _String)) >> exitFailure
                                Just w -> do
                                    -- Location
                                    case w ^? key "location" of
                                        Nothing -> putStrLn "Error: 地点未找到" >> exitFailure
                                        Just location -> do
                                            putStr $ (fmt "城市: ") ++ maybe "" unpack (location ^? key "name" . _String)
                                            putStrLn $ " - " ++ maybe "" unpack (location ^? key "region" . _String)
                                            -- putStrLn $ " - " ++ maybe "" unpack (location ^? key "country" . _String)
                                            putStrLn $ (fmt "当地时间: ") ++ maybe "" unpack (location ^? key "localtime" . _String)
                                    -- Current
                                    case w ^? key "current" of
                                        Nothing -> putStrLn "Error: 未找到当前天气信息" >> exitFailure
                                        Just current -> do
                                            putStrLn $ (fmt "温度: ") ++ maybe "" show (current ^? key "temp_c" . _Number) ++ "℃"
                                            putStrLn $ (fmt "风速: ") ++ maybe "" show (current ^? key "wind_kph" . _Number) ++ " km/h"
                                            putStrLn $ (fmt "云量: ") ++ maybe "" show (current ^? key "cloud" . _Number) ++ "%"
                                    -- Forecast
                                    case w ^? key "forecast". key "forecastday". nth 0. key "day" of
                                        Nothing -> putStrLn "Error: 未找到预报信息" >> exitFailure
                                        Just forecast -> do
                                            putStr $ (fmt "日间气温: ") ++ maybe "" show (forecast ^? key "mintemp_c" . _Number) ++ "℃"
                                            putStrLn $ " - " ++ maybe "" show (forecast ^? key "maxtemp_c" . _Number) ++ "℃"
                                            putStrLn $ (fmt "日平均气温: ") ++ maybe "" show (forecast ^? key "avgtemp_c". _Number) ++ "℃"
                                            putStrLn $ (fmt "总降雨量: ") ++ maybe "" show (forecast ^? key "totalprecip_mm". _Number) ++ "mm"
                                            putStrLn $ (fmt "总降雪量: ") ++ maybe "" show (forecast ^? key "totalsnow_cm". _Number) ++ "mm"
                        "en" -> case decode (response ^. W.responseBody) :: Maybe Value of
                            Nothing -> putStrLn "Error: decoding failed" >> exitFailure
                            Just w -> do
                                -- Location
                                case w ^? key "location" of
                                    Nothing -> putStrLn "Error: location not found" >> exitFailure
                                    Just location -> do
                                        putStr $ (fmt "City: ") ++ maybe "" unpack (location ^? key "name" . _String)
                                        putStrLn $ " - " ++ maybe "" unpack (location ^? key "region" . _String)
                                        -- putStrLn $ " - " ++ maybe "" unpack (location ^? key "country" . _String)
                                        putStrLn $ (fmt "LocalTime: ") ++ maybe "" unpack (location ^? key "localtime" . _String)
                                -- Current
                                case w ^? key "current" of
                                    Nothing -> putStrLn "Error: current not found" >> exitFailure
                                    Just current -> do
                                        putStrLn $ (fmt "Temperature: ") ++ maybe "" show (current ^? key "temp_c" . _Number) ++ "℃"
                                        putStrLn $ (fmt "Wind: ") ++ maybe "" show (current ^? key "wind_kph" . _Number) ++ " km/h"
                                        putStrLn $ (fmt "Cloud: ") ++ maybe "" show (current ^? key "cloud" . _Number) ++ "%"
                                -- Forecast
                                case w ^? key "forecast". key "forecastday". nth 0. key "day" of
                                    Nothing -> putStrLn "Error: forecast not found" >> exitFailure
                                    Just forecast -> do
                                        putStr $ (fmt "DayTemRange: ") ++ maybe "" show (forecast ^? key "mintemp_c" . _Number) ++ "℃"
                                        putStrLn $ " - " ++ maybe "" show (forecast ^? key "maxtemp_c" . _Number) ++ "℃"
                                        putStrLn $ (fmt "DayAvgTem: ") ++ maybe "" show (forecast ^? key "avgtemp_c". _Number) ++ "℃"
                                        putStrLn $ (fmt "TotalPrincp: ") ++ maybe "" show (forecast ^? key "totalprecip_mm". _Number) ++ "mm"
                                        putStrLn $ (fmt "TotalSnow: ") ++ maybe "" show (forecast ^? key "totalsnow_cm". _Number) ++ "mm"
                        _ -> error "Invalid language code" >> exitFailure

