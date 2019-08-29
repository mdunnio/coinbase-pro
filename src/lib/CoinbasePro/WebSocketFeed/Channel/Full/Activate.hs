{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module CoinbasePro.WebSocketFeed.Channel.Full.Activate
    ( Activate (..)
    ) where

import           Data.Aeson.Casing (snakeCase)
import           Data.Aeson.TH     (defaultOptions, deriveJSON,
                                    fieldLabelModifier)
import           Data.Text         (Text)
import           Data.Time.Clock   (UTCTime)

import           CoinbasePro.Types (Funds, OrderId, Price, ProductId, Side,
                                    Size)


type UserId       = Int
type ProfileId    = Text
type StopType     = Text
type TakerFeeRate = Double


data Activate = Activate
    { productId    :: ProductId
    , timestamp    :: UTCTime
    , userId       :: UserId
    , profileId    :: ProfileId
    , orderId      :: OrderId
    , stopType     :: StopType
    , side         :: Side
    , stopPrice    :: Price
    , size         :: Size
    , funds        :: Funds
    , takerFeeRate :: TakerFeeRate
    , private      :: Bool
    } deriving (Eq, Ord, Show)


deriveJSON defaultOptions {fieldLabelModifier = snakeCase} ''Activate
