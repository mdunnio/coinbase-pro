module CoinbasePro.WebSocketFeed
    ( parseFeed
    ) where

import           Data.Aeson                        (eitherDecode')
import           Data.Either                       (either)
import qualified Network.WebSockets                as WS

import           CoinbasePro.WebSocketFeed.Channel (ChannelMessage (..))


parseFeed :: WS.Connection -> IO ChannelMessage
parseFeed conn = either fail return =<< (eitherDecode' <$> WS.receiveData conn :: IO (Either String ChannelMessage))
