{-# LANGUAGE OverloadedStrings, FlexibleContexts, TypeFamilies, QuasiQuotes, TemplateHaskell #-}
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

data WeatherField = WeatherField T.Text Float deriving (Show)

instance FromRow WeatherField where
  fromRow = WeatherField <$> field <*> field 

instance ToRow WeatherField where
  toRow (WeatherField theDate temp) = toRow (theDate, temp)

show :: Show a => a -> Text
show = pack . P.show

data Sitemap
    = Home
    | DayQuery Text

$(derivePathInfo ''Sitemap)

siteRoute :: Connection -> Sitemap -> RouteT Sitemap (ServerPartT IO) Response
siteRoute conn url = 
    case url of
        Home                  -> appRoot
        (DayQuery d)          -> dayQueryHandler conn d 

convRender :: (url -> [(Text, Maybe Text)] -> Text)
           -> (url -> [(Text, Text)]-> Text)
convRender maybeF url params = maybeF url $ map (\(t1, t2) -> (t1, Just t2)) params

renderFunction :: MonadRoute m => m (URL m -> [(Text, Text)] -> Text)
renderFunction = fmap convRender askRouteFn

appRoot :: RouteT Sitemap (ServerPartT IO) Response
appRoot = do
    urlF <- renderFunction
    ok $ toResponse $ ([hamlet|
    Hi
    |]) urlF

dayQueryHandler :: Connection -> Text -> RouteT Sitemap (ServerPartT IO) Response
dayQueryHandler conn d = do
  urlF <- renderFunction
  r <- liftIO $ (queryNamed conn "SELECT the_date, temperature FROM weather WHERE the_date = :dt" [":dt" := show d] :: IO [WeatherField])
  liftIO (putStrLn $ show r)
  ok $ toResponse $ ([hamlet|
    dayquery
    |]) urlF

sitemapSite :: Connection -> Site Sitemap (ServerPartT IO Response)
sitemapSite conn = setDefault Home $ mkSitePI (runRouteT (siteRoute conn)) 

main :: IO()
main = do
    conn <- open "data/np-weather.db"
    simpleHTTP nullConf $ msum [
      implSite "http://localhost:8000" "/site" (sitemapSite conn)
      ]
