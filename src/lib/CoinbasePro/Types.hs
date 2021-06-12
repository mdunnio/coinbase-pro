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
    , UserId
    , ProfileId
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
    , CurrencyType (..)
    , Currency (..)
    , CryptoAddress (..)

    , filterOrderFieldName
    ) where

import           Data.Aeson            (FromJSON, FromJSONKey, ToJSON,
                                        ToJSONKey, parseJSON, toJSON, withArray,
                                        withObject, withText, (.:), (.:?))
import qualified Data.Aeson            as A
import           Data.Aeson.Casing     (camelCase, snakeCase)
import           Data.Aeson.TH         (constructorTagModifier, defaultOptions,
                                        deriveJSON, fieldLabelModifier,
                                        unwrapUnaryRecords)
import           Data.Text             (Text, pack, toLower, unpack)
import           Data.Time.Clock       (UTCTime)
import           Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import           Data.UUID             (UUID, toString, toText)
import qualified Data.Vector           as V
import           Servant.API
import           Text.Printf           (printf)


type UserId    = Text
type ProfileId = Text
type Sequence  = Int


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
    deriving (Eq, Ord, ToHttpApiData)


instance Show OrderId where
  show (OrderId t) = unpack t


deriveJSON defaultOptions
    { fieldLabelModifier = snakeCase
    , unwrapUnaryRecords = True
    } ''OrderId


newtype ClientOrderId = ClientOrderId { unClientOrderId :: UUID }
    deriving (Eq, Ord)


instance Show ClientOrderId where
  show (ClientOrderId t) = toString t


instance ToHttpApiData ClientOrderId where
    toUrlPiece = ("client:" <>) . toText . unClientOrderId


deriveJSON defaultOptions
    { unwrapUnaryRecords = True
    } ''ClientOrderId


newtype ProductId = ProductId { unProductId :: Text }
    deriving (Eq, Ord, ToHttpApiData, ToJSONKey, FromJSONKey)


instance Show ProductId where
  show (ProductId t) = unpack t


deriveJSON defaultOptions
    { fieldLabelModifier = snakeCase
    , unwrapUnaryRecords = True
    } ''ProductId


newtype Price = Price { unPrice :: Double }
    deriving (Eq, Ord, ToHttpApiData)


instance Show Price where
  show (Price d) = printf "%.8f" d


instance FromJSON Price where
    parseJSON = withText "price" $ \t ->
      return . Price . read $ unpack t


instance ToJSON Price where
    toJSON = A.String . pack . show


newtype Size = Size { unSize :: Double }
    deriving (Eq, Ord, Num, ToHttpApiData)


instance Show Size where
  show (Size d) = printf "%.8f" d


instance ToJSON Size where
    toJSON = A.String . pack . show


instance FromJSON Size where
    parseJSON = withText "size" $ \t ->
      return . Size . read $ unpack t


newtype Volume = Volume { unVolume :: Double }
    deriving (Eq, Ord)


instance Show Volume where
  show (Volume d) = printf "%.8f" d


instance FromJSON Volume where
    parseJSON = withText "volume" $ \t ->
      return . Volume . read $ unpack t


instance ToJSON Volume where
    toJSON = A.String . pack . show


newtype TradeId = TradeId Int
    deriving (Eq, Show)


deriveJSON defaultOptions { fieldLabelModifier = snakeCase } ''TradeId


newtype Funds = Funds Double
    deriving (Eq, Ord, ToHttpApiData)


instance Show Funds where
  show (Funds d) = printf "%.16f" d


instance ToJSON Funds where
    toJSON = A.String . pack . show


instance FromJSON Funds where
    parseJSON = withText "funds" $ \t ->
      return . Funds . read $ unpack t


data OrderType = Limit | Market
    deriving (Eq, Ord, Show)


instance ToHttpApiData OrderType where
    toUrlPiece   = toLower . pack . show
    toQueryParam = toLower . pack . show


deriveJSON defaultOptions {constructorTagModifier = camelCase} ''OrderType


newtype CreatedAt = CreatedAt UTCTime
    deriving (Eq, Show, Ord)


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
    , maxWithdrawalAmount  :: Maybe Double
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
        <*> o .:? "max_withdrawal_amount"


newtype CurrencyType = CurrencyType Text
    deriving (Eq, Ord, ToHttpApiData, FromJSONKey)


instance Show CurrencyType where
    show (CurrencyType c) = unpack c


deriveJSON defaultOptions
    { fieldLabelModifier = snakeCase
    , unwrapUnaryRecords = True
    } ''CurrencyType


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


newtype CryptoAddress = CryptoAddress Text
    deriving ToHttpApiData


instance Show CryptoAddress where
    show (CryptoAddress ca) = unpack ca


deriveJSON defaultOptions
    { fieldLabelModifier = snakeCase
    , unwrapUnaryRecords = True
    } ''CryptoAddress
