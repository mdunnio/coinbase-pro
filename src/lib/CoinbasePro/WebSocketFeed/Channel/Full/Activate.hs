{-# LANGUAGE OverloadedStrings #-}

module CoinbasePro.WebSocketFeed.Channel.Full.Activate
    ( Activate (..)
    ) where

import           Data.Aeson        (FromJSON (..), withObject, (.:))
import           Data.Text         (Text)
import           Data.Time.Clock   (UTCTime)

import           CoinbasePro.Types (OrderId, Price, ProductId, Side, Size)


data Activate = Activate
    { productId    :: ProductId
    , timestamp    :: UTCTime
    , userId       :: Int
    , profileId    :: Text
    , orderId      :: OrderId
    , stopType     :: Text
    , side         :: Side
    , stopPrice    :: Price
    , size         :: Size
    , funds        :: Double
    , takerFeeRate :: Double
    , private      :: Bool
    } deriving (Eq, Ord, Show)


instance FromJSON Activate where
    parseJSON = withObject "activate" $ \o -> do
        prid  <- o .: "product_id"
        ti    <- o .: "time"
        uid   <- o .: "user_id"
        prfid <- o .: "profile_id"
        oid   <- o .: "order_id"
        st    <- o .: "stop_type"
        s     <- o .: "side"
        sp    <- o .: "stop_price"
        sz    <- o .: "size"
        f     <- o .: "funds"
        tfr   <- o .: "taker_fee_rate"
        pr    <- o .: "private"
        return $ Activate prid ti uid prfid oid st s (read sp) (read sz) (read f) (read tfr) pr
