{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module CoinbasePro.WebSocketFeed.Channel.Full.Received
    ( Received (..)
    ) where

import           Data.Aeson.Casing (snakeCase)
import           Data.Aeson.TH     (defaultOptions, deriveJSON,
                                    fieldLabelModifier)
import           Data.Time.Clock   (UTCTime)

import           CoinbasePro.Types (Funds, OrderId, OrderType, Price, ProductId,
                                    Sequence, Side, Size)


data Received = Received
    { time      :: UTCTime
    , productId :: ProductId
    , sequence  :: Sequence
    , orderId   :: OrderId
    , size      :: Maybe Size
    , price     :: Maybe Price
    , funds     :: Maybe Funds
    , side      :: Side
    , orderType :: OrderType
    } deriving (Eq, Ord, Show)


deriveJSON defaultOptions {fieldLabelModifier = snakeCase} ''Received


-- instance FromJSON Received where
--     parseJSON = withObject "received" $ \o -> do
--         ti   <- o .: "time"
--         prid <- o .: "product_id"
--         sq   <- o .: "sequence"
--         oid  <- o .: "order_id"
--         sz   <- o .:? "size"
--         p    <- o .:? "price"
--         f    <- o .:? "funds"
--         s    <- o .: "side"
--         ot   <- o .: "order_type"
--         return $ Received ti prid sq oid (read <$> sz) (read <$> p) (read <$> f) s ot
