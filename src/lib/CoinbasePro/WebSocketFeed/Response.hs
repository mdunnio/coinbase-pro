{-# LANGUAGE OverloadedStrings #-}

module CoinbasePro.WebSocketFeed.Response
    ( ResponseMessageType(..)
    , ResponseChannel(..)
    , Subscription (..)
    ) where


import           Data.Aeson                        (FromJSON (..), withObject,
                                                    withText, (.:))
import           Data.Aeson.Types                  (typeMismatch)
import           Data.Text                         (Text)

import           CoinbasePro.WebSocketFeed.Request (ChannelName (..))


data ResponseMessageType = Subscriptions
    deriving (Eq, Ord)


instance Show ResponseMessageType where
    show Subscriptions = "subscriptions"


instance FromJSON ResponseMessageType where
    parseJSON v = withText "response message type" (\t ->
      case t of
        "subscriptions" -> return Subscriptions
        _               -> typeMismatch "response message type" v) v


data ResponseChannel = ResponseChannel
    { respChanName       :: ChannelName
    , respChanProductIds :: [Text]
    } deriving (Eq, Ord, Show)


instance FromJSON ResponseChannel where
    parseJSON = withObject "response channel" $ \o ->
      ResponseChannel <$>
          o .: "name" <*>
          o .: "product_ids"


data Subscription = Subscription
    { respMsgType  :: ResponseMessageType
    , respChannels :: [ResponseChannel]
    } deriving (Eq, Ord, Show)


instance FromJSON Subscription where
    parseJSON = withObject "subscription" $ \o ->
      Subscription <$>
        o .: "type" <*>
        o .: "channels"
