{-# LANGUAGE OverloadedStrings #-}

module CoinbasePro.WebSocketFeed.Channel.Full.Received
    ( Received (..)
    ) where

import           Data.Aeson        (FromJSON (..), withObject, (.:), (.:?))
import           Data.Text         (Text)
import           Data.Time.Clock   (UTCTime)

import           CoinbasePro.Types (OrderId, Price, ProductId, Sequence, Side,
                                    Size)


-- TODO: Split out Limit and Market order Received types
data Received = Received
    { time      :: UTCTime
    , productId :: ProductId
    , sequence  :: Sequence
    , orderId   :: OrderId
    , size      :: Maybe Size
    , price     :: Maybe Price
    , funds     :: Maybe Double
    , side      :: Side
    , orderType :: Text
    } deriving (Eq, Ord, Show)


instance FromJSON Received where
    parseJSON = withObject "received" $ \o -> do
        ti   <- o .: "time"
        prid <- o .: "product_id"
        sq   <- o .: "sequence"
        oid  <- o .: "order_id"
        sz   <- o .:? "size"
        p    <- o .:? "price"
        f    <- o .:? "funds"
        s    <- o .: "side"
        ot   <- o .: "order_type"
        return $ Received ti prid sq oid (read <$> sz) (read <$> p) (read <$> f) s ot
