{-# LANGUAGE OverloadedStrings, FlexibleContexts, TypeFamilies, QuasiQuotes, TemplateHaskell #-}

-- Happstack + WebRoutes + Shakespeare demo from Tazjin's blog https://tazj.in/en/1335123720

module Main where

import Prelude hiding    (reverse, show)
import Control.Monad     (liftM, msum)
import Data.List         (sort)
import Data.Text         (Text, pack, unpack, reverse, toUpper)
import Happstack.Server
import Text.Hamlet
import Text.Lucius
import Web.Routes
import Web.Routes.Happstack
import Web.Routes.TH

import qualified Prelude as P

show :: Show a => a -> Text
show = pack . P.show

data EchoType = Reverse | Sort | Upper

data Sitemap
    = Home
    | Plus Int Int
    | Echo EchoType Text

$(derivePathInfo ''EchoType)
$(derivePathInfo ''Sitemap)

siteRoute :: Sitemap -> RouteT Sitemap (ServerPartT IO) Response
siteRoute url = 
    case url of
        Home         -> appRoot
        (Plus i1 i2) -> plusHandler i1 i2
        (Echo et t)  -> echoHandler et t

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

stylesheet :: Css
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
    <p>Also other boring stuff, for example #
        <a href=@{Echo Reverse "Penguins!"}>this.
    |]) urlF

plusHandler n1 n2 = do 
    urlF <- renderFunction
    ok $ toResponse $ siteLayout ([hamlet|
    <p>Input numbers are #{n1} and #{n2}
    <p>Sum is: #{n1 + n2}
    |]) urlF

echoHandler et t = do
    urlF <- renderFunction
    ok $ toResponse $ siteLayout ([hamlet|
    <p>Input is #{t}
    $case et
        $of Reverse
            <p>Method is "reverse"
            <p>Result is #{reverse t}
        $of Upper
            <p>Method is "upper"
            <p>Result is #{toUpper t}
        $of Sort
            <p>Method is "sort"
            <p>Result is #{pack $ sort $ unpack t}
    |]) urlF

sitemapSite :: Site Sitemap (ServerPartT IO Response)
sitemapSite = setDefault Home $ mkSitePI (runRouteT siteRoute) 

main :: IO()
main = simpleHTTP nullConf $ msum [
      do dirs "site.css" nullDir
         setHeaderM "Content-Type" "text/css"
         ok $ toResponse $ renderCss stylesheet
    , implSite "http://localhost:8000" "/site" sitemapSite
    , seeOther ("/site" :: String) (toResponse ())
    ]
