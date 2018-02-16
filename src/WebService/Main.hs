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
    | Plus Int Int

$(derivePathInfo ''Sitemap)

siteRoute :: Connection -> Sitemap -> RouteT Sitemap (ServerPartT IO) Response
siteRoute conn url = 
    case url of
        Home             -> appRoot
        (Date d)         -> dayHandler d conn 
        (Range d1 d2)    -> rangeHandler d1 d2 conn 
        (Plus i1 i2)     -> plusHandler i1 i2

siteLayout :: HtmlUrl Sitemap -> HtmlUrl Sitemap
siteLayout body = [hamlet|
$doctype 5
<html>
  <head>
    <link href="/site.css" rel="stylesheet" media="all" type="text/css">
  <body>
    <div class="container">
      ^{body}
      <p><a href=@{Home}>Home</a>
|]

stylesheet = [lucius|
body {
    background: #EBEBEB;
    height: 100%;
}
.container {
    padding: 20px;
}
|] undefined -- no URL rendering in this css

convRender :: (url -> [(Text, Maybe Text)] -> Text)
           -> (url -> [(Text, Text)]-> Text)
convRender maybeF url params = maybeF url $ map (\(t1, t2) -> (t1, Just t2)) params

renderFunction :: MonadRoute m => m (URL m -> [(Text, Text)] -> Text)
renderFunction = fmap convRender askRouteFn

appRoot = do
    urlF <- renderFunction
    ok $ toResponse $ siteLayout ([hamlet|
    <p>Small demo application for Happstack, web-routes, Hamlet and Lucius.
    <p>Add two numbers by going to /plus/int1/int2, e.g. 
        <a href=@{Plus 13 37}> here.
    <p>Also other boring stuff.
    |]) urlF

plusHandler n1 n2 = do 
    urlF <- renderFunction
    ok $ toResponse $ siteLayout ([hamlet|
    <p>Input numbers are #{n1} and #{n2}
    <p>Sum is: #{n1 + n2}
    |]) urlF

dayHandler :: Text -> Connection -> RouteT Sitemap (ServerPartT IO) Response
dayHandler d conn = do
  liftIO $ putStrLn ("Looking for: " ++ P.show d)
  urlF <- renderFunction
  r <- liftIO $ (queryNamed conn "SELECT the_date, temperature FROM weather WHERE the_date = :dt" [":dt" := d] :: IO [WeatherField])
  liftIO (putStrLn (BC.unpack $ Data.Aeson.encode r))
  let res = if null r then "NO DATA" else BC.unpack $ Data.Aeson.encode $ head r
  ok $ toResponse $ siteLayout ([hamlet|
    #{res}
    |]) urlF

rangeHandler :: Text -> Text -> Connection -> RouteT Sitemap (ServerPartT IO) Response
rangeHandler d1 d2 conn = do
  liftIO $ putStrLn ("Looking for: " ++ P.show d1 ++ "/" ++ P.show d2)
  urlF <- renderFunction
  r <- liftIO (queryNamed conn "SELECT the_date, temperature FROM weather WHERE the_date >= :d1 AND the_date <= :d2"
       [":d1" := d1, ":d2" := d2] :: IO [WeatherField])
  let res = if null r then "NO DATA" else concat (map (BC.unpack . Data.Aeson.encode) r)
  ok $ toResponse $ siteLayout ([hamlet|
    #{res}
    |]) urlF
    
sitemapSite :: Connection -> Site Sitemap (ServerPartT IO Response)
sitemapSite conn = setDefault Home $ mkSitePI (runRouteT (siteRoute conn)) 

main :: IO()
main = do
    conn <- open "data/np-weather.db"
    r <- query_ conn "SELECT the_date, temperature FROM weather" :: IO [WeatherField]
    mapM_ (\x -> putStrLn $ P.show x) r
    simpleHTTP nullConf $ msum [
      do dirs "site.css" nullDir
         setHeaderM "Content-Type" "text/css"
         ok $ toResponse $ renderCss stylesheet
      , implSite "http://localhost:8000" "/site" (sitemapSite conn)
      , seeOther ("/site" :: String) (toResponse ())
      ]
