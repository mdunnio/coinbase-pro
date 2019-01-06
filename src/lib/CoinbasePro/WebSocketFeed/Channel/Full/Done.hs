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

import           CoinbasePro.Types (OrderId, Price, ProductId, Sequence, Side,
                                    Size)


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
    } deriving (Eq, Ord, Show)


deriveJSON defaultOptions {fieldLabelModifier = snakeCase} ''Done


-- instance FromJSON Done where
--     parseJSON = withObject "done" $ \o -> do
--       ti   <- o .: "time"
--       prid <- o .: "product_id"
--       sq   <- o .: "sequence"
--       p    <- o .:? "price"
--       oid  <- o .: "order_id"
--       re   <- o .: "reason"
--       s    <- o .: "side"
--       rs   <- o .:? "remaining_size"
--       return $ Done ti prid sq (read <$> p) oid re s (read <$> rs)
