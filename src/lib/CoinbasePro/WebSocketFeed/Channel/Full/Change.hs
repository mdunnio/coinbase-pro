{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module CoinbasePro.WebSocketFeed.Channel.Full.Change
    ( Change (..)
    ) where

import           Data.Aeson.Casing (snakeCase)
import           Data.Aeson.TH     (defaultOptions, deriveJSON,
                                    fieldLabelModifier)
import           Data.Time.Clock   (UTCTime)

import           CoinbasePro.Types (Funds, OrderId, Price, ProductId, Sequence,
                                    Side, Size)


data Change = Change
    { time      :: UTCTime
    , sequence  :: Sequence
    , orderId   :: OrderId
    , productId :: ProductId
    , newSize   :: Maybe Size
    , oldSize   :: Maybe Size
    , newFunds  :: Maybe Funds
    , oldFunds  :: Maybe Funds
    , price     :: Maybe Price
    , side      :: Side
    } deriving (Eq, Ord, Show)


deriveJSON defaultOptions {fieldLabelModifier = snakeCase} ''Change
