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

main :: IO ()
main = do
    response <- call_api myAPI localCity
    let mwc = decode (response ^. responseBody) :: Maybe WeatherCurrent
    case mwc of
        Nothing -> putStrLn "Error: decoding failed"
        Just w -> do
            putStrLn $ show w

