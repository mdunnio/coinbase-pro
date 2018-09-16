{-# LANGUAGE OverloadedStrings #-}

module CoinbasePro.MarketData.AggregateOrderBook
    ( AggregateOrderBook(..)
    , AggregateLevel(..)
    ) where

import           Data.Aeson  (FromJSON (..), withArray, withObject, (.:))
import qualified Data.Vector as V
import           Data.Word   (Word64)


data AggregateOrderBook = AggregateOrderBook
    { sequence :: Int
    , bids     :: [AggregateLevel]
    , asks     :: [AggregateLevel]
    } deriving (Eq, Ord, Show)


instance FromJSON AggregateOrderBook where
    parseJSON = withObject "aggregate orderbook" $ \o ->
        AggregateOrderBook <$>
            o .: "sequence" <*>
            o .: "bids" <*>
            o .: "asks"


data AggregateLevel = AggregateLevel
    { price     :: Double
    , size      :: Double
    , numOrders :: Word64
    } deriving (Eq, Ord, Show)


instance FromJSON AggregateLevel where
    parseJSON = withArray "level" $ \a -> do
        let l = V.toList a
        p   <- parseJSON $ head l
        sz  <- parseJSON $ l !! 1
        nos <- parseJSON $ l !! 2
        return $ AggregateLevel (read p) (read sz) nos
