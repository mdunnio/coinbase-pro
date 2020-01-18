{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TemplateHaskell            #-}

module CoinbasePro.Types
    ( OrderId (..)
    , ClientOrderId (..)
    , Price (..)
    , ProductId (..)
    , Sequence
    , Side (..)
    , Size (..)
    , Volume (..)
    , TradeId (..)
    , Funds
    , OrderType (..)
    , CreatedAt (..)
    , Candle (..)
    , CandleGranularity (..)
    , TwentyFourHourStats (..)
    , Currency (..)

    , filterOrderFieldName
    ) where

import           Data.Aeson            (FromJSON, ToJSON, parseJSON, toJSON,
                                        withArray, withObject, withText, (.:),
                                        (.:?))
import qualified Data.Aeson            as A
import           Data.Aeson.Casing     (camelCase, snakeCase)
import           Data.Aeson.TH         (constructorTagModifier, defaultOptions,
                                        deriveJSON, fieldLabelModifier,
                                        unwrapUnaryRecords)
import           Data.Text             (Text, pack, toLower, unpack)
import           Data.Time.Clock       (UTCTime)
import           Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import           Data.UUID             (UUID)
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


newtype ClientOrderId = ClientOrderId { unClientOrderId :: UUID }
    deriving (Eq, Ord, Show, ToHttpApiData)


deriveJSON defaultOptions
    { unwrapUnaryRecords = True
    } ''ClientOrderId


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


newtype Volume = Volume { unVolume :: Double }
    deriving (Eq, Ord, Show)


instance FromJSON Volume where
    parseJSON = withText "volume" $ \t ->
      return . Volume . read $ unpack t


instance ToJSON Volume where
    toJSON (Volume v) = A.String . pack $ printf "%.8f" v


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


data TwentyFourHourStats = TwentyFourHourStats
    { open24   :: Price
    , high24   :: Price
    , low24    :: Price
    , volume24 :: Volume
    , last24   :: Price
    , volume30 :: Volume
    } deriving (Eq, Show)


deriveJSON defaultOptions { fieldLabelModifier = init . init } ''TwentyFourHourStats


data CurrencyDetails = CurrencyDetails
    { cdType               :: Text
    , symbol               :: Maybe Text
    , networkConfirmations :: Maybe Int
    , sortOrder            :: Maybe Int
    , cryptoAddressLink    :: Maybe Text
    , pushPaymentMethods   :: [Text]
    , groupTypes           :: Maybe [Text]
    , maxPrecision         :: Maybe Double
    } deriving (Eq, Show)


instance FromJSON CurrencyDetails where
    parseJSON = withObject "currency details" $ \o -> CurrencyDetails
        <$> o .: "type"
        <*> o .:? "symbol"
        <*> o .:? "network_confirmations"
        <*> o .:? "set_order"
        <*> o .:? "crypto_address_link"
        <*> o .: "push_payment_methods"
        <*> o .:? "group_types"
        <*> o .:? "max_precision"


data Currency = Currency
    { id      :: Text
    , name    :: Text
    , minSize :: Double
    , status  :: Text
    , message :: Maybe Text
    , details :: CurrencyDetails
    } deriving (Eq, Show)


instance FromJSON Currency where
    parseJSON = withObject "currency" $ \o -> Currency
        <$> o .: "id"
        <*> o .: "name"
        <*> (read <$> o .: "min_size")
        <*> o .: "status"
        <*> o .:? "message"
        <*> o .: "details"
