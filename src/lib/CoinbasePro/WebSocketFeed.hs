module CoinbasePro.WebSocketFeed
    ( subscribeToFeed
    , subscribeToSandboxFeed
    ) where

import           Control.Concurrent                 (forkIO)
import           Control.Monad                      (forever)
import           Control.Monad.IO.Class             (liftIO)
import           Data.Aeson                         (eitherDecode', encode)
import           Data.Either                        (either)
import           Data.Maybe                         (maybe)
import           Network.HTTP.Types                 (methodGet)
import           Network.Socket                     (HostName)
import           Network.Socket.Internal            (PortNumber)
import qualified Network.WebSockets                 as WS
import qualified System.IO.Streams                  as Streams
import           System.IO.Streams.Concurrent.Unagi (makeChanPipe)
import qualified Wuss                               as WU

import           CoinbasePro.Authenticated.Request  (CoinbaseProCredentials (..),
                                                     mkCBAccessSign,
                                                     mkCBAccessTimeStamp)
import           CoinbasePro.Types                  (ProductId)
import           CoinbasePro.WebSocketFeed.Channel  (ChannelMessage (..))
import           CoinbasePro.WebSocketFeed.Request  (AuthenticatedWebSocketFeedRequest (..),
                                                     ChannelName (..),
                                                     RequestMessageType (..),
                                                     WebSocketFeedRequest (..),
                                                     wsEndpoint,
                                                     wsSandboxEndpoint)
import qualified CoinbasePro.WebSocketFeed.Request  as WR


subscribeToFeed :: [ProductId] -> [ChannelName] -> Maybe CoinbaseProCredentials -> IO (Streams.InputStream ChannelMessage)
subscribeToFeed = subscribe wsHost wsPort
  where
    wsHost = WR.host wsEndpoint
    wsPort = WR.port wsEndpoint


subscribeToSandboxFeed :: [ProductId] -> [ChannelName] -> Maybe CoinbaseProCredentials -> IO (Streams.InputStream ChannelMessage)
subscribeToSandboxFeed = subscribe wsHost wsPort
  where
    wsHost = WR.host wsSandboxEndpoint
    wsPort = WR.port wsSandboxEndpoint


subscribe :: HostName -> PortNumber -> [ProductId] -> [ChannelName] -> Maybe CoinbaseProCredentials -> IO (Streams.InputStream ChannelMessage)
subscribe wsHost wsPort prids channels cpc = do
    (is, os) <- makeChanPipe
    req      <- mkWsRequest cpc

    _ <- forkIO . WU.runSecureClient wsHost wsPort "/" $ \conn -> do
        WS.sendTextData conn req
        forever $ parseFeed conn >>= Streams.writeTo os . Just

    return is
  where
    mkWsRequest = maybe (return $ encode wsRequest) (fmap encode . authWsRequest)

    wsRequest = WebSocketFeedRequest Subscribe prids channels

    authWsRequest cpc' = do
        ts <- liftIO mkCBAccessTimeStamp
        let cbs = mkCBAccessSign (cbSecretKey cpc') ts methodGet authSubscriptionPath ""
        return $ AuthenticatedWebSocketFeedRequest wsRequest cbs (cbAccessKey cpc') (cbAccessPassphrase cpc') ts

    authSubscriptionPath = "/users/self/verify"


parseFeed :: WS.Connection -> IO ChannelMessage
parseFeed conn = either fail return =<< (eitherDecode' <$> WS.receiveData conn :: IO (Either String ChannelMessage))
