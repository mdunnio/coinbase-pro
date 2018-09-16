{-# LANGUAGE OverloadedStrings #-}

module CoinbasePro.MarketData.Types
    ( Product (..)
    , CBTime (..)
    ) where

import           Data.Aeson      (FromJSON (..), withObject, (.:))
import           Data.Text       (Text)
import           Data.Time.Clock (UTCTime)


data Product = Product
    { id             :: Text
    , baseCurrency   :: Text
    , quoteCurrency  :: Text
    , baseMinSize    :: Double
    , baseMaxSize    :: Double
    , quoteIncrement :: Double
    } deriving (Eq, Show)


instance FromJSON Product where
    parseJSON = withObject "product" $ \o -> do
        prid  <- o .: "id"
        bc    <- o .: "base_currency"
        qc    <- o .: "quote_currency"
        bmins <- o .: "base_min_size"
        bmaxs <- o .: "base_max_size"
        qi    <- o .: "quote_increment"
        return $ Product prid bc qc (read bmins) (read bmaxs) (read qi)


newtype CBTime = CBTime { unCBTime :: UTCTime } deriving (Eq, Show)


instance FromJSON CBTime where
    parseJSON = withObject "time" $ \o ->
      CBTime <$> o .: "iso"
