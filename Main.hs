{-# LANGUAGE OverloadedStrings #-}

import Control.Applicative ((<$>), (<*>), empty)
import Control.Monad (forever, when)
import Data.Aeson
import Data.Maybe (fromJust)
import System.Environment (getArgs)
import System.Exit (exitFailure)
import System.IO (hPutStrLn, stderr)
import System.ZMQ4.Monadic (Rep(..), connect, liftIO, receive, runZMQ, send', socket)

data Request = Request
  { requestId      :: String
  , requestRequest :: String
  , requestBody    :: String
  } deriving (Show)

data Response = Response
  { responseId      :: String
  , responseRequest :: String
  , responseBody    :: String
  } deriving (Show)

instance FromJSON Request where
  parseJSON (Object v) = Request
    <$> v .: "id"
    <*> v .: "request"
    <*> v .: "data"
  parseJSON _ = empty

instance ToJSON Response where
  toJSON (Response id response body) = object
    [ "id"       .= id
    , "response" .= response
    , "data"     .= body
    ]

main :: IO ()
main = do
  args <- getArgs

  when (length args < 1) $ do
    hPutStrLn stderr "usage: display <address>"
    exitFailure

  runZMQ $ do
    s <- socket Rep
    connect s $ args !! 0
    forever $ do
      json <- receive s
      let request = decodeStrict json
      handleRequest s $ fromJust request

handleRequest s request = do
  liftIO . print $ request
  let response = Response (requestId request) "completed" (reverse . requestBody $ request)
  liftIO . print $ response
  let json = encode response
  send' s [] json
