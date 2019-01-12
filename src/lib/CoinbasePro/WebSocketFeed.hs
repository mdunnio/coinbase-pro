module CoinbasePro.WebSocketFeed
    ( subscribeToFeed
    ) where

import           Control.Concurrent                 (forkIO)
import           Control.Monad                      (forever)
import           Data.Aeson                         (eitherDecode', encode)
import           Data.Either                        (either)
import qualified Network.WebSockets                 as WS
import qualified System.IO.Streams                  as Streams
import           System.IO.Streams.Concurrent.Unagi (makeChanPipe)
import qualified Wuss                               as WU

import           CoinbasePro.Types                  (ProductId)
import           CoinbasePro.WebSocketFeed.Channel  (ChannelMessage (..))
import           CoinbasePro.WebSocketFeed.Request  (ChannelName (..),
                                                     RequestMessageType (..),
                                                     WebSocketFeedRequest (..),
                                                     wsEndpoint)
import qualified CoinbasePro.WebSocketFeed.Request  as WR


subscribeToFeed :: [ProductId] -> IO (Streams.InputStream ChannelMessage)
subscribeToFeed prids = do
    (is, os) <- makeChanPipe
    _ <- forkIO . WU.runSecureClient wsHost wsPort "/" $ \conn -> do
        WS.sendTextData conn $ encode request
        forever $ parseFeed conn >>= Streams.writeTo os . Just
    return is
  where
    wsHost = WR.host wsEndpoint
    wsPort = WR.port wsEndpoint

    request = WebSocketFeedRequest Subscribe prids [Full]


parseFeed :: WS.Connection -> IO ChannelMessage
parseFeed conn = either fail return =<< (eitherDecode' <$> WS.receiveData conn :: IO (Either String ChannelMessage))
