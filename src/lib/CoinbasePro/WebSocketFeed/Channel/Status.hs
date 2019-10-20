{-# LANGUAGE OverloadedStrings #-}

module CoinbasePro.WebSocketFeed.Channel.Status
    ( Status (..)
    ) where

import           Data.Aeson                   (FromJSON, parseJSON, withObject,
                                               (.:))
import           Data.Aeson.Casing            (snakeCase)
import           Data.Aeson.TH                (defaultOptions, deriveJSON,
                                               fieldLabelModifier)
import           Data.Text                    (Text)

import           CoinbasePro.MarketData.Types (Product)
import           CoinbasePro.Types            (Currency)


data Status = Status
    { currencies :: [Currency]
    , products   :: [Product]
    } deriving (Eq, Show)


instance FromJSON Status where
    parseJSON = withObject "status" $ \o -> Status
        <$> o .: "currencies"
        <*> o .: "products"
