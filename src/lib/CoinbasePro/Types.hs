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
    , TradeId (..)
    , Funds
    , OrderType (..)
    , CreatedAt (..)
    , Candle (..)
    , CandleGranularity (..)

    , filterOrderFieldName
    ) where

import           Data.Aeson            (FromJSON, ToJSON, parseJSON, toJSON,
                                        withArray, withText)
import qualified Data.Aeson            as A
import           Data.Aeson.Casing     (camelCase, snakeCase)
import           Data.Aeson.TH         (constructorTagModifier, defaultOptions,
                                        deriveJSON, fieldLabelModifier,
                                        unwrapUnaryRecords)
import           Data.Text             (Text, pack, toLower, unpack)
import           Data.Time.Clock       (UTCTime)
import           Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import qualified Data.Vector           as V
import           Servant.API
import           Text.Printf           (printf)


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


newtype TradeId = TradeId Int
    deriving (Eq, Show)


deriveJSON defaultOptions { fieldLabelModifier = snakeCase } ''TradeId


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


newtype CreatedAt = CreatedAt UTCTime
    deriving (Eq, Show)


deriveJSON defaultOptions ''CreatedAt


filterOrderFieldName :: String -> String
filterOrderFieldName "order_type" = "type"
filterOrderFieldName s            = s


data Candle = Candle
    { time   :: UTCTime
    , low    :: Price
    , high   :: Price
    , open   :: Price
    , close  :: Price
    , volume :: Double
    } deriving (Eq, Show)


instance FromJSON Candle where
    parseJSON = withArray "candle" $ \a -> do
      let l = V.toList a
      t  <- posixSecondsToUTCTime <$> parseJSON (head l)
      lw <- Price <$> parseJSON (l !! 1)
      h  <- Price <$> parseJSON (l !! 2)
      o  <- Price <$> parseJSON (l !! 3)
      c  <- Price <$> parseJSON (l !! 4)
      v  <- parseJSON $ l !! 5
      return $ Candle t lw h o c v


data CandleGranularity = Minute | FiveMinutes | FifteenMinutes | Hour | SixHours | Day
    deriving (Eq, Ord, Show)


instance ToHttpApiData CandleGranularity where
    toUrlPiece Minute         = "60"
    toUrlPiece FiveMinutes    = "300"
    toUrlPiece FifteenMinutes = "900"
    toUrlPiece Hour           = "3600"
    toUrlPiece SixHours       = "21600"
    toUrlPiece Day            = "86400"

    toQueryParam Minute         = "60"
    toQueryParam FiveMinutes    = "300"
    toQueryParam FifteenMinutes = "900"
    toQueryParam Hour           = "3600"
    toQueryParam SixHours       = "21600"
    toQueryParam Day            = "86400"
