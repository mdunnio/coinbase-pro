{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module CoinbasePro.WebSocketFeed.Channel.Full.Open
    ( Open (..)
    ) where

import           Data.Aeson.Casing (snakeCase)
import           Data.Aeson.TH     (defaultOptions, deriveJSON,
                                    fieldLabelModifier)
import           Data.Time.Clock   (UTCTime)

import           CoinbasePro.Types (OrderId, Price, ProductId, ProfileId,
                                    Sequence, Side, Size, UserId)


data Open = Open
    { time          :: UTCTime
    , productId     :: ProductId
    , sequence      :: Sequence
    , orderId       :: OrderId
    , price         :: Price
    , remainingSize :: Maybe Size
    , side          :: Side
    , userId        :: Maybe UserId
    , profileId     :: Maybe ProfileId
    } deriving (Eq, Ord, Show)


deriveJSON defaultOptions {fieldLabelModifier = snakeCase} ''Open
