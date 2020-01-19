{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TemplateHaskell            #-}

module CoinbasePro.WebSocketFeed.Channel.Full.Done
    ( Done (..)
    ) where

import           Data.Aeson        (FromJSON, parseJSON)
import           Data.Aeson.Casing (snakeCase)
import           Data.Aeson.TH     (defaultOptions, deriveJSON,
                                    fieldLabelModifier)
import           Data.Text         (Text)
import           Data.Time.Clock   (UTCTime)

import           CoinbasePro.Types (OrderId, Price, ProductId, ProfileId,
                                    Sequence, Side, Size, UserId)


type Reason = Text

newtype RemainingSize = RemainingSize { unRemainingSize :: Maybe Size }
    deriving (Eq, Ord, Show)


instance FromJSON RemainingSize where
    parseJSON = (RemainingSize <$>) . parseJSON


data Done = Done
    { time          :: UTCTime
    , productId     :: ProductId
    , sequence      :: Sequence
    , price         :: Maybe Price
    , orderId       :: OrderId
    , reason        :: Reason
    , side          :: Side
    , remainingSize :: Maybe Size
    , userId        :: Maybe UserId
    , profileId     :: Maybe ProfileId
    } deriving (Eq, Ord, Show)


deriveJSON defaultOptions {fieldLabelModifier = snakeCase} ''Done
