module Main where

import Control.Monad    (msum)
import Data.Char        (toLower)
import Happstack.Server (FromReqURI(..), dirs, dir, path, seeOther, nullConf, simpleHTTP, toResponse, ok)
import Control.Monad.Trans
import Control.Monad
import System.IO
import Database.HDBC
import Database.HDBC.MySQL
import qualified Data.Text as T
import Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Database.SQLite.Simple           as S

---------sqlite in-memory version -------------------
-- | Create database schema - we use in-memory database
initDb :: S.Connection -> IO S.Connection
initDb c = do
    S.execute_ c "create table post_code (address text, code int)"
    S.execute_ c "insert into post_code (address, code) values ('Grafton','x1010')"
    return c

-- | Handler - finds postcode for known addresses
hFind :: Handler App App ()
hFind = do
    address <- fromJust <$> getParam "address"
    addr <- fromBS $ address
    r <- query "select cast(code as text) from post_code where address = ?" (addr :: Address) :: Handler App App [Postcode]
    writeText $ (TX.pack . show) r

-----------------------------------------------------
{- 
Write a web service that allows people to get weather data for

+ a single day
+ a date range FROM TO -- full data or averages

/weatherAPI/query/YYYY-mm-dd
/weatherAPI/query/range/YYYY-mm-dd/YYYY-mm-dd/

and PUT data for a new day

Write some tests using http-test

import Test.HTTP
import Data.List (isInfixOf)

main = defaultMain $ httpTestCase "BayesHive landing page" "https://bayeshive.com" $ do
    landing <- get "/"
    assert "Correct blog link" $
      "href=\"https://bayeshive.com/blog\"" `isInfixOf` landing
    loginResult <- postForm "/auth/page/email/login"
                     [("email", "foo"), ("password", "bar")]
    debug loginResult

-}
-- Representation of a person with firstname/lastname
data PersonName = 
     PersonName { firstName :: String
                , lastName  :: String
                } deriving (Eq, Show)

-- Converts SQL row into PersonName data
intoPersonName :: [SqlValue] -> PersonName
intoPersonName [firstName, lastName] =
    PersonName first last
    where first = (fromSql firstName) :: String
          last  = (fromSql lastName) :: String

-- Converts PersonName data into JSON
instance ToJSON PersonName where
    toJSON (PersonName fn ln) = object [ (T.pack "firstname") .= fn, (T.pack "lastname") .= ln ]

-- Extract one row from the database and converts it into JSON string
getFromDb :: Connection -> IO String
getFromDb conn = do 
    rows <- quickQuery' conn "SELECT * from persons" []
    let reply3 = head $ map intoPersonName rows
    return (BL.unpack $ encode reply3)

-- Fires up a happstack server and connects to a local MySQL server
main :: IO ()
main = do
    conn <- connectMySQL defaultMySQLConnectInfo {
        mysqlUser     = "testing",
        mysqlPassword = "testing1234",
        mysqlDatabase = "test"
    }
    str <- (getFromDb conn)
    simpleHTTP nullConf $ 
        msum [ dirs "db" $ ok str 
        , seeOther "db" "db"
        ]
