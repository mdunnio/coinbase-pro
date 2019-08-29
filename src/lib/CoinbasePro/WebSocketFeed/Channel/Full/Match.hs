{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module CoinbasePro.WebSocketFeed.Channel.Full.Match
    ( Match (..)
    ) where

import           Data.Aeson.Casing (snakeCase)
import           Data.Aeson.TH     (defaultOptions, deriveJSON,
                                    fieldLabelModifier)
import           Data.Time.Clock   (UTCTime)

import           CoinbasePro.Types (OrderId, Price, ProductId, Sequence, Side,
                                    Size)


type TradeId = Int


data Match = Match
    { tradeId      :: TradeId
    , sequence     :: Sequence
    , makerOrderId :: OrderId
    , takerOrderId :: OrderId
    , time         :: UTCTime
    , productId    :: ProductId
    , size         :: Size
    , price        :: Price
    , side         :: Side
    } deriving (Eq, Ord, Show)


deriveJSON defaultOptions {fieldLabelModifier = snakeCase} ''Match
