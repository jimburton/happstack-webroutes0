{-# LANGUAGE OverloadedStrings, FlexibleContexts, TypeFamilies, QuasiQuotes, TemplateHaskell, DeriveGeneric #-}
module Main where

import Prelude hiding    (reverse, show)
import Control.Monad     (liftM, msum)
import Control.Monad.IO.Class (liftIO)
import Data.List         (sort)
import Data.Text         (Text, pack, unpack, reverse, toUpper)
import Happstack.Server
import Text.Hamlet
import Text.Lucius
import Web.Routes
import Web.Routes.Happstack
import Web.Routes.TH
import           Database.SQLite.Simple
import           Database.SQLite.Simple.FromRow
import Data.Dates
import qualified Data.Text as T
import qualified Prelude as P
import Data.Aeson
import GHC.Generics (Generic )
import qualified Data.ByteString.Lazy as LB
import qualified Data.ByteString.Lazy.Char8 as BC

data WeatherField = WeatherField {date :: T.Text, temperature :: Float} deriving (Generic, Eq, Show)

instance FromRow WeatherField where
  fromRow = WeatherField <$> field <*> field 

instance ToRow WeatherField where
  toRow (WeatherField theDate temp) = toRow (theDate, temp)

instance ToJSON   WeatherField
instance FromJSON WeatherField

show :: Show a => a -> Text
show = pack . P.show

data Sitemap
    = Home
    | Date Text
    | Range Text Text

$(derivePathInfo ''Sitemap)

siteRoute :: Connection -> Sitemap -> RouteT Sitemap (ServerPartT IO) Response
siteRoute conn url = 
    case url of
        Home             -> appRoot
        (Date d)         -> dayHandler d conn 
        (Range d1 d2)    -> rangeHandler d1 d2 conn 

convRender :: (url -> [(Text, Maybe Text)] -> Text)
           -> (url -> [(Text, Text)]-> Text)
convRender maybeF url params = maybeF url $ map (\(t1, t2) -> (t1, Just t2)) params

renderFunction :: MonadRoute m => m (URL m -> [(Text, Text)] -> Text)
renderFunction = fmap convRender askRouteFn

appRoot = do
    urlF <- renderFunction
    ok $ toResponse $ ([hamlet|
$doctype 5
<html>
  <head>
  <body>
    Small demo webservice using Happstack, web-routes and Julius.
    Find the weather for a given date <a href=@{Date "2017-01-01"}> here.
    Find the weather between two dates <a href=@{Range "2017-01-01" "2017-12-31"}> here.
    |]) urlF

dayHandler :: Text -> Connection -> RouteT Sitemap (ServerPartT IO) Response
dayHandler d conn = do
  r <- liftIO $ (queryNamed conn "SELECT the_date, temperature \
                                \ FROM weather \
                                \ WHERE the_date = :dt" [":dt" := d] :: IO [WeatherField])
  let res = if null r then "NO DATA" else BC.unpack $ Data.Aeson.encode $ head r
  ok $ toResponse res

rangeHandler :: Text -> Text -> Connection -> RouteT Sitemap (ServerPartT IO) Response
rangeHandler d1 d2 conn = do
  r <- liftIO (queryNamed conn "SELECT the_date, temperature \
                              \ FROM weather \
                              \ WHERE the_date >= :d1 \
                              \ AND the_date <= :d2"
       [":d1" := d1, ":d2" := d2] :: IO [WeatherField])
  let res = if null r then "NO DATA" else concat (map (BC.unpack . Data.Aeson.encode) r)
  ok $ toResponse res
    
sitemapSite :: Connection -> Site Sitemap (ServerPartT IO Response)
sitemapSite conn = setDefault Home $ mkSitePI (runRouteT (siteRoute conn)) 

main :: IO()
main = do
    conn <- open "data/np-weather.db"
    simpleHTTP nullConf $ implSite "http://localhost:8000" "/weather" (sitemapSite conn)
      
