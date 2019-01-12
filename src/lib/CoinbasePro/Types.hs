{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TemplateHaskell            #-}

module CoinbasePro.Types
    ( OrderId (..)
    , Price (..)
    , ProductId (..)
    , Sequence
    , Side (..)
    , Size (..)
    , Funds
    , OrderType (..)
    , CreatedAt (..)

    , RequiredHeader
    , UserAgent

    , userAgent

    , filterOrderFieldName
    ) where

import           Data.Aeson        (FromJSON, ToJSON, parseJSON, toJSON,
                                    withText)
import qualified Data.Aeson        as A
import           Data.Aeson.Casing (camelCase, snakeCase)
import           Data.Aeson.TH     (constructorTagModifier, defaultOptions,
                                    deriveJSON, fieldLabelModifier,
                                    unwrapUnaryRecords)
import           Data.Text         (Text, pack, toLower, unpack)
import           Data.Time.Clock   (UTCTime)
import           Servant.API
import           Text.Printf       (printf)


type Sequence = Int

data Side = Buy | Sell
    deriving (Eq, Ord, Show)


instance ToHttpApiData Side where
    toUrlPiece   = toLower . pack . show
    toQueryParam = toLower . pack . show


deriveJSON defaultOptions
    { constructorTagModifier = camelCase
    , fieldLabelModifier     = snakeCase
    } ''Side


newtype OrderId = OrderId { unOrderId :: Text }
    deriving (Eq, Ord, Show, ToHttpApiData)


deriveJSON defaultOptions
    { fieldLabelModifier = snakeCase
    , unwrapUnaryRecords = True
    } ''OrderId


newtype ProductId = ProductId { unProductId :: Text }
    deriving (Eq, Ord, Show, ToHttpApiData)


deriveJSON defaultOptions
    { fieldLabelModifier = snakeCase
    , unwrapUnaryRecords = True
    } ''ProductId


newtype Price = Price { unPrice :: Double }
    deriving (Eq, Ord, Show, ToHttpApiData)


instance FromJSON Price where
    parseJSON = withText "price" $ \t ->
      return . Price . read $ unpack t


instance ToJSON Price where
    toJSON (Price p) = A.String . pack $ printf "%.8f" p


newtype Size = Size { unSize :: Double }
    deriving (Eq, Ord, Show, ToHttpApiData)


instance ToJSON Size where
    toJSON (Size s) = A.String . pack $ printf "%.8f" s


instance FromJSON Size where
    parseJSON = withText "size" $ \t ->
      return . Size . read $ unpack t


newtype Funds = Funds { unFunds :: Double }
    deriving (Eq, Ord, Show, ToHttpApiData)


instance ToJSON Funds where
    toJSON (Funds s) = A.String . pack $ printf "%.16f" s


instance FromJSON Funds where
    parseJSON = withText "size" $ \t ->
      return . Funds . read $ unpack t


data OrderType = Limit | Market
    deriving (Eq, Ord, Show)


instance ToHttpApiData OrderType where
    toUrlPiece   = toLower . pack . show
    toQueryParam = toLower . pack . show


deriveJSON defaultOptions {constructorTagModifier = camelCase} ''OrderType


type RequiredHeader = Header' '[Required]


newtype UserAgent = UserAgent Text
    deriving (Eq, Show, ToHttpApiData)


userAgent :: UserAgent
userAgent = UserAgent "coinbase-pro/0.1"


newtype CreatedAt = CreatedAt UTCTime
    deriving (Eq, Show)


deriveJSON defaultOptions ''CreatedAt


filterOrderFieldName :: String -> String
filterOrderFieldName "order_type" = "type"
filterOrderFieldName s            = s
