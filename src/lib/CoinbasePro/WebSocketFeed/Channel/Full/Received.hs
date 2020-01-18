{-# LANGUAGE OverloadedStrings #-}

module CoinbasePro.WebSocketFeed.Channel.Full.Received
    ( Received (..)
    ) where

import           Data.Aeson        (FromJSON (..), withObject, (.:), (.:?))
import           Data.Time.Clock   (UTCTime)
import           Data.UUID         (fromString)

import           CoinbasePro.Types (ClientOrderId (..), Funds, OrderId,
                                    OrderType, Price, ProductId, Sequence, Side,
                                    Size)


data Received = Received
    { time      :: UTCTime
    , productId :: ProductId
    , sequence  :: Sequence
    , orderId   :: OrderId
    , clientOid :: Maybe ClientOrderId
    , size      :: Maybe Size
    , price     :: Maybe Price
    , funds     :: Maybe Funds
    , side      :: Side
    , orderType :: OrderType
    } deriving (Eq, Ord, Show)


instance FromJSON Received where
    parseJSON = withObject "received" $ \o -> do
        t     <- o .: "time"
        prd   <- o .: "product_id"
        sq    <- o .: "sequence"
        oid   <- o .: "order_id"
        cloid <- (ClientOrderId <$>) . fromString <$> (o .: "client_oid")
        sz    <- o .:? "size"
        px    <- o .:? "price"
        fs    <- o .:? "funds"
        sd    <- o .: "side"
        ot    <- o .: "order_type"
        return $ Received t prd sq oid cloid sz px fs sd ot
