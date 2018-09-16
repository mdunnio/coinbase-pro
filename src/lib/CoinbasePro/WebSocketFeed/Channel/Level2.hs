{-# LANGUAGE OverloadedStrings #-}

module CoinbasePro.WebSocketFeed.Channel.Level2
    ( Snapshot (..)
    , SnapshotLevel (..)
    , Change (..)
    , L2Update(..)
    ) where

import           Data.Aeson  (FromJSON (..), withArray, withObject, (.:))
import           Data.Text   (Text)
import qualified Data.Vector as V


data SnapshotLevel = SnapshotLevel
    { price :: Double
    , size  :: Double
    } deriving (Eq, Ord, Show)


instance FromJSON SnapshotLevel where
    parseJSON = withArray "snapshot level" $ \a -> do
        let l = V.toList a
        p  <- parseJSON $ head l
        sz <- parseJSON $ l !! 1
        return $ SnapshotLevel (read p) (read sz)


data Snapshot = Snapshot
    { sProductId :: Text
    , bids       :: [SnapshotLevel]
    , asks       :: [SnapshotLevel]
    } deriving (Eq, Ord, Show)


instance FromJSON Snapshot where
    parseJSON = withObject "snapshot" $ \o ->
      Snapshot <$>
        o .: "product_id" <*>
        o .: "bids" <*>
        o .: "asks"


data Change = Change
    { side   :: Text
    , cPrice :: Double
    , cSize  :: Double
    } deriving (Eq, Ord, Show)


instance FromJSON Change where
    parseJSON = withArray "change" $ \a -> do
        let l = V.toList a
        sd <- parseJSON $ head l
        p  <- parseJSON $ l !! 1
        sz <- parseJSON $ l !! 2
        return $ Change sd (read p) (read sz)


data L2Update = L2Update
    { l2ProductId :: Text
    , changes     :: [Change]
    } deriving (Eq, Ord, Show)


instance FromJSON L2Update where
    parseJSON = withObject "l2update" $ \o ->
      L2Update <$>
        o .: "product_id" <*>
        o .: "changes"
