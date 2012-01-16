{-# LANGUAGE OverloadedStrings, FlexibleContexts #-}

module Googl
  ( shorten
  , expand
  ) where

import Control.Applicative ((<$>), (<*>), empty)
import Control.Monad
import Data.Aeson
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as L
import Data.Conduit
import Network.HTTP.Conduit
import Network.HTTP.Types
import Control.Monad.Trans.Control
import Control.Monad.IO.Class (MonadIO)
import Control.Failure (Failure)

cAPI_URL = "https://www.googleapis.com/urlshortener/v1/url"

data APIResponse = APIResponse
  { kind :: ByteString
  , shortUrl :: ByteString
  , longUrl :: ByteString
  , status :: Maybe ByteString
  } deriving (Show)

instance FromJSON APIResponse where
  parseJSON (Object v) =
    APIResponse <$>
    v .: "kind" <*>
    v .: "id" <*>
    v .: "longUrl" <*>
    v .:? "status"
  parseJSON _ = mzero


request :: ResourceIO m => Request m -> m L.ByteString
request req = do
  res <- withManager $ httpLbs req
  return $ responseBody res

get :: String -> IO L.ByteString
get url = do
  req <- parseUrl url
  let option = req { method = "GET"
                   , requestHeaders = [("Content-Type", "application/json")]
                   }
  request option

post :: String -> ByteString -> IO L.ByteString
post url opt = do
  req <- parseUrl url
  let option = req { method = "POST"
                   , requestHeaders = [("Content-Type", "application/json")]
                   , requestBody = RequestBodyBS $ opt
                   }
  request option

shorten :: String -> IO (Maybe APIResponse)
shorten url = post cAPI_URL opt >>= return . decode
  where
    opt = BS.pack $ concat ["{\"longUrl\": \"", url, "\"}"]

expand :: String -> IO (Maybe APIResponse)
expand url = get opt >>= return . decode
  where
    opt = concat [cAPI_URL, "?shortUrl=", url]

main :: IO ()
main = do
  shorten "http://www.google.com/" >>= print
  expand "http://goo.gl/fbsS" >>= print
