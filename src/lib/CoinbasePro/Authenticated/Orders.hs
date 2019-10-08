{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell            #-}

module CoinbasePro.Authenticated.Orders
  ( Status (..)
  , Statuses (..)
  , Order (..)
  , STP (..)
  , TimeInForce (..)
  , PlaceOrderBody (..)

  , statuses
  ) where


import           Data.Aeson        (FromJSON, ToJSON, parseJSON, withText)
import           Data.Aeson.Casing (snakeCase)
import           Data.Aeson.TH     (constructorTagModifier, defaultOptions,
                                    deriveJSON, fieldLabelModifier,
                                    omitNothingFields)
import qualified Data.Char         as Char
import           Data.Set          (Set, fromList)
import           Data.Text         (pack, toLower, unpack)
import           Web.HttpApiData   (ToHttpApiData (..))

import           CoinbasePro.Types (CreatedAt, OrderId, OrderType, Price,
                                    ProductId, Side, Size, filterOrderFieldName)


-- TODO: All is not a status
data Status = Open | Pending | Active | Done | All
    deriving (Eq, Ord, Show)


instance ToHttpApiData Status where
    toUrlPiece   = toLower . pack . show
    toQueryParam = toLower . pack . show


newtype Statuses = Statuses { unStatuses :: Set Status }
    deriving (Eq, Show)


statuses :: [Status] -> Statuses
statuses = Statuses . fromList


data TimeInForce = GTC | GTT | IOC | FOK
    deriving (Eq, Ord, Show)


instance ToHttpApiData TimeInForce where
    toUrlPiece   = toLower . pack . show
    toQueryParam = toLower . pack . show


data STP = DC | CO | CN | CB
    deriving (Eq, Ord, Show)


instance ToHttpApiData STP where
    toUrlPiece   = toLower . pack . show
    toQueryParam = toLower . pack . show


deriveJSON defaultOptions {constructorTagModifier = fmap Char.toLower} ''Status
deriveJSON defaultOptions ''TimeInForce
deriveJSON defaultOptions {constructorTagModifier = fmap Char.toLower} ''STP


newtype FillFees = FillFees { unFillFees :: Double }
    deriving (Eq, Ord, Show, ToJSON)


instance FromJSON FillFees where
    parseJSON = withText "fill_fees" $ \t ->
      return . FillFees . read $ unpack t


newtype ExecutedValue = ExecutedValue { unExecutedValue :: Double }
    deriving (Eq, Ord, Show, ToJSON)


instance FromJSON ExecutedValue where
    parseJSON = withText "executed_value" $ \t ->
      return . ExecutedValue . read $ unpack t


-- TODO: This might need to be split up into different order types.
data Order = Order
    { id            :: OrderId
    , price         :: Maybe Price
    , size          :: Maybe Size
    , productId     :: ProductId
    , side          :: Side
    , stp           :: Maybe STP
    , orderType     :: OrderType
    , timeInForce   :: Maybe TimeInForce
    , postOnly      :: Bool
    , createdAt     :: CreatedAt
    , fillFees      :: FillFees
    , filledSize    :: Size
    , executedValue :: Maybe ExecutedValue
    , status        :: Status
    , settled       :: Bool
    } deriving (Eq, Show)


deriveJSON defaultOptions {fieldLabelModifier = filterOrderFieldName . snakeCase} ''Order


data PlaceOrderBody = PlaceOrderBody
    { bProductId :: ProductId
    , bSide      :: Side
    , bSize      :: Size
    , bPrice     :: Price
    , bPostOnly  :: Bool
    , bOrderType :: Maybe OrderType
    , bStp       :: Maybe STP
    , bTif       :: Maybe TimeInForce
    } deriving (Eq, Show)


deriveJSON defaultOptions {fieldLabelModifier = snakeCase . drop 1, omitNothingFields = True} ''PlaceOrderBody
