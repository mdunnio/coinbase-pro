module CoinbasePro.WebSocketFeed
    ( subscribeToFeed
    ) where

import           Control.Concurrent                 (forkIO)
import           Control.Monad                      (forever)
import           Control.Monad.IO.Class             (liftIO)
import           Data.Aeson                         (eitherDecode', encode)
import           Network.HTTP.Types                 (methodGet)
import qualified Network.WebSockets                 as WS
import qualified System.IO.Streams                  as Streams
import           System.IO.Streams.Concurrent.Unagi (makeChanPipe)
import qualified Wuss                               as WU

import           CoinbasePro.Authenticated.Request  (CoinbaseProCredentials (..),
                                                     mkCBAccessSign,
                                                     mkCBAccessTimeStamp)
import           CoinbasePro.Environment            (Environment,
                                                     WSConnection (..),
                                                     wsEndpoint)
import           CoinbasePro.Types                  (ProductId)
import           CoinbasePro.WebSocketFeed.Channel  (ChannelMessage (..))
import           CoinbasePro.WebSocketFeed.Request  (AuthenticatedWebSocketFeedRequest (..),
                                                     ChannelName (..),
                                                     RequestMessageType (..),
                                                     WebSocketFeedRequest (..))


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

    mkWsRequest = maybe (return $ encode wsRequest) (fmap encode . authWsRequest)

    wsRequest = WebSocketFeedRequest Subscribe prids channels

    authWsRequest cpc' = do
        ts <- liftIO mkCBAccessTimeStamp
        let cbs = mkCBAccessSign (cbSecretKey cpc') ts methodGet authSubscriptionPath ""
        return $ AuthenticatedWebSocketFeedRequest wsRequest cbs (cbAccessKey cpc') (cbAccessPassphrase cpc') ts

    authSubscriptionPath = "/users/self/verify"


parseFeed :: WS.Connection -> IO ChannelMessage
parseFeed conn = either fail return =<< (eitherDecode' <$> WS.receiveData conn :: IO (Either String ChannelMessage))
