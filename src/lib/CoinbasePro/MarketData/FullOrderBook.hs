{-# LANGUAGE OverloadedStrings #-}

module CoinbasePro.MarketData.FullOrderBook
    ( FullOrderBook (..)
    , Order (..)
    ) where

import           Data.Aeson        (FromJSON (..), withArray, withObject, (.:))
import qualified Data.Vector       as V

import           CoinbasePro.Types (OrderId (..), Price, Size)


data FullOrderBook = FullOrderBook
    { sequence :: Int
    , bids     :: [Order]
    , asks     :: [Order]
    } deriving (Eq, Ord, Show)


instance FromJSON FullOrderBook where
    parseJSON = withObject "orderbook" $ \o ->
        FullOrderBook <$>
            o .: "sequence" <*>
            o .: "bids" <*>
            o .: "asks"


data Order = Order
    { price   :: Price
    , size    :: Size
    , orderId :: OrderId
    } deriving (Eq, Ord, Show)


instance FromJSON Order where
    parseJSON = withArray "order" $ \a -> do
        let l = V.toList a
        px  <- parseJSON $ head l
        sz  <- parseJSON $ l !! 1
        oid <- parseJSON $ l !! 2
        return $ Order px sz oid
