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

import           CoinbasePro.Types (Funds, OrderId, Price, ProductId, ProfileId,
                                    Side, Size, UserId)


type StopType     = Text


data Activate = Activate
    { productId :: ProductId
    , timestamp :: UTCTime
    , orderId   :: OrderId
    , stopType  :: StopType
    , side      :: Side
    , stopPrice :: Price
    , size      :: Size
    , funds     :: Funds
    , private   :: Bool
    , userId    :: UserId
    , profileId :: ProfileId
    } deriving (Eq, Ord, Show)


deriveJSON defaultOptions {fieldLabelModifier = snakeCase} ''Activate
