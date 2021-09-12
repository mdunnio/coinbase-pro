{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell            #-}

module CoinbasePro.Authenticated.Orders
  ( Status (..)
  , Statuses (..)
  , Order (..)
  , STP (..)
  , TimeInForce (..)
  , PlaceOrderBody (..)
  , StopLossSide (..)

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

import           CoinbasePro.Types (ClientOrderId, CreatedAt, OrderId,
                                    OrderType, Price, ProductId, Side, Size, Funds,
                                    filterOrderFieldName)


-- TODO: All is not a status
data Status = Open | Pending | Active | Done | All
    deriving (Eq, Ord)


instance Show Status where
  show Open    = "open"
  show Pending = "pending"
  show Active  = "active"
  show Done    = "done"
  show All     = "all"


instance ToHttpApiData Status where
    toUrlPiece   = pack . show
    toQueryParam = pack . show


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

data StopLossSide = Loss | Entry
    deriving (Eq, Ord, Show)


instance ToHttpApiData StopLossSide where
    toUrlPiece   = toLower . pack . show
    toQueryParam = toLower . pack . show


deriveJSON defaultOptions {constructorTagModifier = fmap Char.toLower} ''StopLossSide


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
    , postOnly      :: Maybe Bool
    , createdAt     :: CreatedAt
    , fillFees      :: FillFees
    , filledSize    :: Size
    , executedValue :: Maybe ExecutedValue
    , status        :: Status
    , settled       :: Bool
    , stop          :: Maybe StopLossSide
    , stopPrice     :: Maybe Price
    } deriving (Eq, Show)


deriveJSON defaultOptions {fieldLabelModifier = filterOrderFieldName . snakeCase} ''Order




data PlaceOrderBody = PlaceOrderBody
    {  bClientOid   :: Maybe ClientOrderId
    ,  bProductId   :: ProductId
    ,  bSide        :: Side
    ,  bSize        :: Maybe Size
    ,  bFunds       :: Maybe Funds
    ,  bPrice       :: Maybe Price
    ,  bPostOnly    :: Maybe Bool
    ,  bOrderType   :: Maybe OrderType
    ,  bStp         :: Maybe STP
    ,  bTimeInForce :: Maybe TimeInForce
    ,  bStop        :: Maybe StopLossSide
    ,  bStopPrice   :: Maybe Price
    } deriving (Eq, Show)

deriveJSON defaultOptions {fieldLabelModifier = snakeCase . drop 1, omitNothingFields = True} ''PlaceOrderBody
