module CoinbasePro.WebSocketFeed
    ( subscribeToFeed
    , subscribeToSandboxFeed
    ) where

import           Control.Concurrent                 (forkIO)
import           Control.Monad                      (forever)
import           Data.Aeson                         (eitherDecode', encode)
import           Data.Either                        (either)
import           Network.Socket                     (HostName)
import           Network.Socket.Internal            (PortNumber)
import qualified Network.WebSockets                 as WS
import qualified System.IO.Streams                  as Streams
import           System.IO.Streams.Concurrent.Unagi (makeChanPipe)
import qualified Wuss                               as WU

import           CoinbasePro.Types                  (ProductId)
import           CoinbasePro.WebSocketFeed.Channel  (ChannelMessage (..))
import           CoinbasePro.WebSocketFeed.Request  (ChannelName (..),
                                                     RequestMessageType (..),
                                                     WebSocketFeedRequest (..),
                                                     wsEndpoint,
                                                     wsSandboxEndpoint)
import qualified CoinbasePro.WebSocketFeed.Request  as WR


subscribeToFeed :: [ProductId] -> [ChannelName] -> IO (Streams.InputStream ChannelMessage)
subscribeToFeed = subscribe wsHost wsPort
  where
    wsHost = WR.host wsEndpoint
    wsPort = WR.port wsEndpoint


subscribeToSandboxFeed :: [ProductId] -> [ChannelName] -> IO (Streams.InputStream ChannelMessage)
subscribeToSandboxFeed = subscribe wsHost wsPort
  where
    wsHost = WR.host wsSandboxEndpoint
    wsPort = WR.port wsSandboxEndpoint


subscribe :: HostName -> PortNumber -> [ProductId] -> [ChannelName] -> IO (Streams.InputStream ChannelMessage)
subscribe wsHost wsPort prids channels = do
    (is, os) <- makeChanPipe
    _ <- forkIO . WU.runSecureClient wsHost wsPort "/" $ \conn -> do
        WS.sendTextData conn $ encode request
        forever $ parseFeed conn >>= Streams.writeTo os . Just
    return is
  where

    request = WebSocketFeedRequest Subscribe prids channels


parseFeed :: WS.Connection -> IO ChannelMessage
parseFeed conn = either fail return =<< (eitherDecode' <$> WS.receiveData conn :: IO (Either String ChannelMessage))
