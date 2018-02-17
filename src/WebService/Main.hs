{-# LANGUAGE OverloadedStrings, FlexibleContexts, TypeFamilies, QuasiQuotes, TemplateHaskell, DeriveGeneric #-}
module Main where

import           System.Log.Logger ( updateGlobalLogger
                                   , rootLoggerName
                                   , setLevel
                                   , debugM
                                   , Priority(..)
                                   )
import           Control.Monad.IO.Class (liftIO)
import           Control.Monad          (msum)
import           Data.List         (intercalate)
import           Data.Text         (Text, pack)
import           Happstack.Server  
import           Web.Routes        
import           Web.Routes.Happstack (implSite)
import           Web.Routes.TH        (derivePathInfo)
import           Database.SQLite.Simple
import           Database.SQLite.Simple.FromRow
import           Data.Aeson
import           GHC.Generics                  (Generic)
import qualified Data.ByteString.Lazy.Char8 as BC

data WeatherField = WeatherField {date :: Text, temperature :: Float}
                    deriving (Generic)

instance FromRow WeatherField where -- ^ Marshal data from DB to our ADT
  fromRow = WeatherField <$> field <*> field 

instance ToRow WeatherField where -- ^ Marshal data from our ADT to the DB
  toRow (WeatherField theDate temp) = toRow (theDate, temp)

instance ToJSON   WeatherField -- ^ Marshal data from our ADT to JSON
instance FromJSON WeatherField -- ^ Marshal data from JSON to our ADT

data Sitemap = Date Text       -- ^ The endpoints in the webservice
             | Range Text Text

$(derivePathInfo ''Sitemap)    -- ^ Turn our ADT into a set of web routes

{-| Handle reuests for a single date. -}
dayHandler :: Text -> Connection -> ServerPart Response
dayHandler d conn = do
  r <- liftIO (queryNamed conn "SELECT the_date, temperature \
                               \ FROM  weather \
                               \ WHERE the_date = :dt" [":dt" := d] :: IO [WeatherField])
  liftIO $ debugM "Date Query" (listToOutput r)
  ok $ toResponse (listToOutput r)

{-| Handle requests for a date range. -}
rangeHandler :: Text -> Text -> Connection -> ServerPart Response
rangeHandler d1 d2 conn = do
  r <- liftIO (queryNamed conn "SELECT the_date, temperature \
                              \ FROM   weather \
                              \ WHERE  the_date >= :d1 \
                              \ AND    the_date <= :d2"
       [":d1" := d1, ":d2" := d2] :: IO [WeatherField])
  liftIO $ debugM "Range Query" (listToOutput r)
  ok $ toResponse (listToOutput r)

{-| Turn a list of WeatherFields into a JSON object. -}
listToOutput :: ToJSON a => [a] -> String
listToOutput xs = "[" ++ intercalate "," (map (BC.unpack . encode) xs) ++ "]"

{-| Entry point. Connects to the database and passes the connection to the
routing function. -}
main :: IO()
main = do
  updateGlobalLogger rootLoggerName (setLevel INFO) -- change level to DEBUG for testing
  conn <- open "data/np-weather.db"
  simpleHTTP nullConf $  do
    setHeaderM "Content-Type" "application/json"
    msum [
      dirs "weather/date" $ path $ \d -> dayHandler d conn
      , dirs "weather/range" $ path $ \d1 -> path $ \d2 -> rangeHandler d1 d2 conn
         ]
