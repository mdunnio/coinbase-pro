{-# LANGUAGE OverloadedStrings #-}

module CoinbasePro.WebSocketFeed.Channel.Full.Open
    ( Open (..)
    ) where

import           Data.Aeson        (FromJSON (..), withObject, (.:), (.:?))
import           Data.Time.Clock   (UTCTime)

import           CoinbasePro.Types (OrderId, Price, ProductId, Sequence, Side,
                                    Size)


data Open = Open
    { time          :: UTCTime
    , productId     :: ProductId
    , sequence      :: Sequence
    , orderId       :: OrderId
    , price         :: Price
    , remainingSize :: Maybe Size
    , side          :: Side
    } deriving (Eq, Ord, Show)


instance FromJSON Open where
    parseJSON = withObject "open" $ \o -> do
      ti   <- o .: "time"
      prid <- o .: "product_id"
      sq   <- o .: "sequence"
      oid  <- o .: "order_id"
      p    <- o .: "price"
      rs   <- o .:? "remaining_size"
      s    <- o .: "side"
      return $ Open ti prid sq oid (read p) (read <$> rs) s
