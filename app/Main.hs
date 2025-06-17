{-# LANGUAGE
    DeriveGeneric, 
    OverloadedStrings #-}
module Main where
import qualified Network.Wreq as W
import qualified Data.ByteString.Lazy.Char8 as BL
import Control.Lens
import Data.Char (ord)
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
showLength :: String -> Int
showLength s = sum $ map countASCII s
    where
        countASCII c
            | ord c <= 127 = 1
            | otherwise = 2

fmt :: Int -> String -> String
fmt l s = replicate padding ' ' ++ s
    where
        padding
            | showLength s > l = 0
            | otherwise = l - showLength s

fmt' :: String -> String
fmt' = fmt 15

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
                    response <- let a = (maybeS (jtoml ^? key "api_key" . _String))
                                    c = if null (city config)
                                        then (maybeS (jtoml ^? key "city" . _String))
                                        else city config
                        in call_api a c
                   -- 语言
                    let trans_lang = if null (lang config)
                                     then (maybeS (jtoml ^? key "default_lang" . _String))
                                     else (lang config)
                    case trans_lang of
                        c | c `elem` ["zh", "en"] -> do
                            let Just trans = (jtoml ^? key "lang". key (pack trans_lang))
                            case decode (response ^. W.responseBody) :: Maybe Value of
                                Nothing -> putStrLn (maybeS (trans ^? key "decode_error". _String)) >> exitFailure
                                Just w -> do
                                    -- Location
                                    case w ^? key "location" of
                                        Nothing -> putStrLn (maybeS (trans ^? key "location". key "not_found". _String)) >> exitFailure
                                        Just location -> do
                                            putStr
                                                $ (fmt' (maybeS (trans ^? key "location". key "location". _String)))
                                                ++ maybeS (location ^? key "name" . _String)
                                            putStrLn
                                                $ " - "
                                                ++ maybeS (location ^? key "region" . _String)
                                            putStrLn
                                                $ (fmt' (maybeS (trans ^? key "location". key "localtime". _String)))
                                                ++ maybeS (location ^? key "localtime" . _String)
                                    -- Current
                                    case w ^? key "current" of
                                        Nothing -> putStrLn (fmt' (maybeS (trans ^? key "current". key "not_found". _String))) >> exitFailure
                                        Just current -> do
                                            putStrLn
                                                $ (fmt' (maybeS (trans ^? key "current". key "temp". _String)))
                                                ++ maybeN (current ^? key "temp_c" . _Number) ++ "℃"
                                            putStrLn
                                                $ (fmt' (maybeS (trans ^? key "current". key "wind". _String)))
                                                ++ maybeN (current ^? key "wind_kph" . _Number) ++ " km/h"
                                            putStrLn
                                                $ (fmt' (maybeS (trans ^? key "current". key "cloud". _String)))
                                                ++ maybeN (current ^? key "cloud" . _Number) ++ "%"
                                    -- Forecast
                                    case w ^? key "forecast". key "forecastday". nth 0. key "day" of
                                        Nothing -> putStrLn (fmt' (maybeS (trans ^? key "forecast". key "not_found". _String))) >> exitFailure
                                        Just forecast -> do
                                            putStr
                                                $ (fmt' (maybeS (trans ^? key "forecast". key "range". _String)))
                                                ++ maybeN (forecast ^? key "mintemp_c" . _Number) ++ "℃"
                                            putStrLn
                                                $ "  - " 
                                                ++ maybeN (forecast ^? key "maxtemp_c" . _Number) ++ "℃"
                                            putStrLn
                                                $ (fmt' (maybeS (trans ^? key "forecast". key "avg". _String)))
                                                ++ maybeN (forecast ^? key "avgtemp_c". _Number) ++ "℃"
                                            putStrLn
                                                $ (fmt' (maybeS (trans ^? key "forecast". key "precip". _String)))
                                                ++ maybeN (forecast ^? key "totalprecip_mm". _Number) ++ "mm"
                                            putStrLn
                                                $ (fmt' (maybeS (trans ^? key "forecast". key "snow". _String)))
                                                ++ maybeN (forecast ^? key "totalsnow_cm". _Number) ++ "mm"
                        _ -> error "Invalid language code" >> exitFailure

