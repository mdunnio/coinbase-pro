{-# LANGUAGE OverloadedStrings #-}

module CoinbasePro.WebSocketFeed
    ( subscribeToFeed
    ) where

import           Control.Concurrent                 (forkIO)
import           Control.Exception                  (Exception, throwIO)
import           Control.Monad                      (forever)
import           Data.Aeson                         (decode', encode)
import qualified Network.WebSockets                 as WS
import qualified System.IO.Streams                  as Streams
import           System.IO.Streams.Concurrent.Unagi (makeChanPipe)
import qualified Wuss                               as WU

import           CoinbasePro.Authenticated.Request  (CoinbaseProCredentials (..))
import           CoinbasePro.Environment            (Environment,
                                                     WSConnection (..),
                                                     wsEndpoint)
import           CoinbasePro.Types                  (ProductId)
import           CoinbasePro.WebSocketFeed.Channel  (ChannelMessage (..))
import           CoinbasePro.WebSocketFeed.Request  (ChannelName (..),
                                                     RequestMessageType (..),
                                                     WebSocketFeedRequest (..),
                                                     authenticatedWebSocketFeedRequest)


data ParseException = ParseException deriving Show
instance Exception ParseException


subscribeToFeed :: [ProductId] -> [ChannelName] -> Environment -> Maybe CoinbaseProCredentials -> IO (Streams.InputStream ChannelMessage)
subscribeToFeed prds channels env = subscribe (wsEndpoint env) prds channels


subscribe :: WSConnection -> [ProductId] -> [ChannelName] -> Maybe CoinbaseProCredentials -> IO (Streams.InputStream ChannelMessage)
subscribe wsConn prids channels cpc = do
    (is, os) <- makeChanPipe
    req      <- mkWsRequest cpc

    _ <- forkIO . WU.runSecureClient wsHost wsPort "/" $ \conn -> do
        WS.sendTextData conn req
        forever $ parseFeed conn >>= Streams.writeTo os . Just

    return is
  where
    wsHost = host wsConn
    wsPort = port wsConn

    mkWsRequest   = maybe (return $ encode wsRequest) (fmap encode . authWsRequest)
    wsRequest     = WebSocketFeedRequest Subscribe prids channels
    authWsRequest = authenticatedWebSocketFeedRequest wsRequest


parseFeed :: WS.Connection -> IO ChannelMessage
parseFeed conn = WS.receiveData conn >>= maybe err return . decode'
  where err = throwIO ParseException
