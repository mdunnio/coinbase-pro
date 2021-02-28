{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module CoinbasePro.Authenticated.Report
  ( ReportRequest (..)
  , ReportResponse (..)

  , FillsReportRequest
  , AccountsReportRequest
  ) where

import           Data.Aeson                         (ToJSON (..), object, (.=))
import           Data.Aeson.Casing                  (snakeCase)
import           Data.Aeson.TH                      (constructorTagModifier,
                                                     defaultOptions, deriveJSON,
                                                     fieldLabelModifier)
import qualified Data.Char                          as Char
import           Data.Text                          (Text)
import           Data.Time.Clock                    (UTCTime)

import           CoinbasePro.Authenticated.Accounts (AccountId)
import           CoinbasePro.Types                  (CreatedAt, ProductId)


data ReportFormat = PDF | CSV
    deriving Show


deriveJSON defaultOptions { constructorTagModifier = fmap Char.toLower } ''ReportFormat


newtype Email = Email Text
    deriving Show


deriveJSON defaultOptions { fieldLabelModifier = snakeCase } ''Email


data Request = Request
    { rStartDate :: UTCTime
    , rEndDate   :: UTCTime
    , rFormat    :: Maybe ReportFormat
    , rEmail     :: Maybe Email
    } deriving Show


data FillsReportRequest = FillsReportRequest
    { frProductId :: ProductId
    , frAccountId :: Maybe AccountId
    , frRequest   :: Request
    } deriving Show


data AccountsReportRequest = AccountsReportRequest
    { arAccountId :: AccountId
    , arProductId :: Maybe ProductId
    , arRequest   :: Request
    } deriving Show


data ReportRequest = Fills FillsReportRequest | Accounts AccountsReportRequest
    deriving Show


instance ToJSON ReportRequest where
    toJSON (Fills frr) =
      object ([ "type"       .= ("fills" :: Text)
              , "start_date" .= rStartDate (frRequest frr)
              , "end_date"   .= rEndDate (frRequest frr)
              , "format"     .= rFormat (frRequest frr)
              , "email"      .= rEmail (frRequest frr)
              , "product_id" .= frProductId frr
              ] <> maybe mempty (\aid -> ["account_id" .= aid]) (frAccountId frr)
             )
    toJSON (Accounts arr) =
      object ([ "type"       .= ("accounts" :: Text)
              , "start_date" .= rStartDate (arRequest arr)
              , "end_date"   .= rEndDate (arRequest arr)
              , "format"     .= rFormat (arRequest arr)
              , "email"      .= rEmail (arRequest arr)
              , "account_id" .= arAccountId arr
              ] <> maybe mempty (\prid -> ["product_id" .= prid]) (arProductId arr)
             )


data ReportRequestType = FillsType | AccountsType


instance Show ReportRequestType where
    show FillsType    = "fills"
    show AccountsType = "accounts"


deriveJSON defaultOptions { fieldLabelModifier = snakeCase . init . init . init . init } ''ReportRequestType


-- instance FromJSON ReportRequestType where
--     parseJSON = withText "report request type" $
--       \case
--         "fills"    -> return FillsType
--         "accounts" -> return AccountsType
--         _          -> fail "parse error"


data ReportStatus = Pending | Creating | Ready


instance Show ReportStatus where
    show Pending  = "pending"
    show Creating = "creating"
    show Ready    = "ready"


deriveJSON defaultOptions { constructorTagModifier = fmap Char.toLower
                          , fieldLabelModifier = snakeCase
                          } ''ReportStatus


data ReportParams = ReportParams
    { startDate :: UTCTime
    , endDate   :: UTCTime
    } deriving Show


deriveJSON defaultOptions { fieldLabelModifier = snakeCase } ''ReportParams


{-|
  {
    "id": "0428b97b-bec1-429e-a94c-59232926778d",
    "type": "fills",
    "status": "pending",
    "created_at": "2015-01-06T10:34:47.000Z",
    "completed_at": undefined,
    "expires_at": "2015-01-13T10:35:47.000Z",
    "file_url": undefined,
    "params": {
        "start_date": "2014-11-01T00:00:00.000Z",
        "end_date": "2014-11-30T23:59:59.000Z"
    }
  }
-}
data ReportResponse = ReportResponse
    { rrId          :: Text
    , rrType        :: ReportRequestType
    , rrStatus      :: ReportStatus
    , rrCreatedAt   :: CreatedAt
    , rrCompletedAt :: UTCTime
    , rrExpiresAt   :: UTCTime
    , rrFileUrl     :: Text
    , rrParams      :: ReportParams
    } deriving Show


deriveJSON defaultOptions { fieldLabelModifier = snakeCase . drop 2 } ''ReportResponse
