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


-- instance FromJSON Activate where
--     parseJSON = withObject "activate" $ \o -> do
--         prid  <- o .: "product_id"
--         ti    <- o .: "time"
--         uid   <- o .: "user_id"
--         prfid <- o .: "profile_id"
--         oid   <- o .: "order_id"
--         st    <- o .: "stop_type"
--         s     <- o .: "side"
--         sp    <- o .: "stop_price"
--         sz    <- o .: "size"
--         f     <- o .: "funds"
--         tfr   <- o .: "taker_fee_rate"
--         pr    <- o .: "private"
--         return $ Activate prid ti uid prfid oid st s (read sp) (read sz) (read f) (read tfr) pr
