{-# LANGUAGE OverloadedStrings #-}

module CoinbasePro.MarketData.OrderBook
    ( OrderBook(..)
    , Order(..)
    ) where

import           Data.Aeson  (FromJSON (..), withArray, withObject, (.:))
import           Data.Text   (Text)
import qualified Data.Vector as V


data OrderBook = OrderBook
    { sequence :: Int
    , bids     :: [Order]
    , asks     :: [Order]
    } deriving (Eq, Ord, Show)


instance FromJSON OrderBook where
    parseJSON = withObject "orderbook" $ \o ->
        OrderBook <$>
            o .: "sequence" <*>
            o .: "bids" <*>
            o .: "asks"


data Order = Order
    { price   :: Double
    , size    :: Double
    , orderId :: Text
    } deriving (Eq, Ord, Show)


instance FromJSON Order where
    parseJSON = withArray "order" $ \a -> do
        let l = V.toList a
        px  <- parseJSON $ head l
        sz  <- parseJSON $ l !! 1
        oid <- parseJSON $ l !! 2
        return $ Order (read px) (read sz) oid
