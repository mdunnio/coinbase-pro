{-# LANGUAGE OverloadedStrings #-}

module CoinbasePro.WebSocketFeed.Channel.Full.Done
    ( Done (..)
    ) where

import           Data.Aeson        (FromJSON (..), withObject, (.:), (.:?))
import           Data.Text         (Text)
import           Data.Time.Clock   (UTCTime)

import           CoinbasePro.Types (OrderId, Price, ProductId, Sequence, Side,
                                    Size)


data Done = Done
    { time          :: UTCTime
    , productId     :: ProductId
    , sequence      :: Sequence
    , price         :: Maybe Price
    , orderId       :: OrderId
    , reason        :: Text
    , side          :: Side
    , remainingSize :: Maybe Size
    } deriving (Eq, Ord, Show)


instance FromJSON Done where
    parseJSON = withObject "done" $ \o -> do
      ti   <- o .: "time"
      prid <- o .: "product_id"
      sq   <- o .: "sequence"
      p    <- o .:? "price"
      oid  <- o .: "order_id"
      re   <- o .: "reason"
      s    <- o .: "side"
      rs   <- o .:? "remaining_size"
      return $ Done ti prid sq (read <$> p) oid re s (read <$> rs)
