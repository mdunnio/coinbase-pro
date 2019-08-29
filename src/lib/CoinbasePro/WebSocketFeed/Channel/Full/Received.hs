{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module CoinbasePro.WebSocketFeed.Channel.Full.Received
    ( Received (..)
    ) where

import           Data.Aeson.Casing (snakeCase)
import           Data.Aeson.TH     (defaultOptions, deriveJSON,
                                    fieldLabelModifier)
import           Data.Time.Clock   (UTCTime)

import           CoinbasePro.Types (Funds, OrderId, OrderType, Price, ProductId,
                                    Sequence, Side, Size)


data Received = Received
    { time      :: UTCTime
    , productId :: ProductId
    , sequence  :: Sequence
    , orderId   :: OrderId
    , size      :: Maybe Size
    , price     :: Maybe Price
    , funds     :: Maybe Funds
    , side      :: Side
    , orderType :: OrderType
    } deriving (Eq, Ord, Show)


deriveJSON defaultOptions {fieldLabelModifier = snakeCase} ''Received
