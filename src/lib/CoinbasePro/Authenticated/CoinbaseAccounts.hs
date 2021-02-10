{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TemplateHaskell            #-}

module CoinbasePro.Authenticated.CoinbaseAccounts
    ( CoinbaseAccount (..)
    ) where

import           Data.Aeson        (FromJSON (..), Value (..), withObject, (.:),
                                    (.:?))
import           Data.Aeson.Casing (snakeCase)
import           Data.Aeson.TH     (defaultOptions, deriveJSON,
                                    fieldLabelModifier)
import           Data.Aeson.Types  (parseMaybe)
import           Data.Text         (Text)
import           Data.UUID         (UUID)


data BankCountry = BankCountry
    { code :: Text
    , name :: Text
    } deriving Show


deriveJSON defaultOptions
    { fieldLabelModifier = snakeCase
    } ''BankCountry


data WireDepositInformation = WireDepositInformation
    { accountNumber  :: Maybe Text
    , routingNumber  :: Text
    , bankName       :: Text
    , bankAddress    :: Text
    , bankCountry    :: BankCountry
    , accountName    :: Text
    , accountAddress :: Text
    , reference      :: Text
    } deriving Show


instance FromJSON WireDepositInformation where
  parseJSON = withObject "wire deposit information" $ \o -> WireDepositInformation
    <$> o .: "account_number"
    <*> o .: "routing_number"
    <*> o .: "bank_name"
    <*> o .: "bank_address"
    <*> (parseJSON =<< (o .: "bank_country"))
    <*> o .: "account_name"
    <*> o .: "account_address"
    <*> o .: "reference"


-- deriveJSON defaultOptions
--     { fieldLabelModifier = snakeCase
--     } ''WireDepositInformation


data SepaDepositInformation = SepaDepositInformation
    { sIban            :: Text
    , sSwift           :: Text
    , sBankName        :: Text
    , sBankAddress     :: Text
    , sBankCountryName :: Text
    , sAccountName     :: Text
    , sAccountAddress  :: Text
    , sReference       :: Text
    } deriving Show


deriveJSON defaultOptions
    { fieldLabelModifier = snakeCase . drop 1
    } ''SepaDepositInformation


data DepositInformation = Wire WireDepositInformation | Sepa SepaDepositInformation
  deriving Show


-- TODO: this is slightly messy, potentially refactor
instance FromJSON DepositInformation where
    parseJSON = withObject "deposit_information" $ \o -> do
      w <- o .:? "wire_deposit_information"
      case w of
        Just (Object w') -> Wire <$> parseJSON (Object w')
        Nothing -> do
          s <- o .:? "sepa_deposit_information"
          case s of
            Just (Object s') -> Sepa <$> parseJSON (Object s')
            _                -> fail "Unable to parse deposit information"
        _ -> fail "Unable to parse deposit information"


data Account = Account
    { aId       :: Text
    , aName     :: Text
    , aBalance  :: Double
    , aCurrency :: Text
    , aType     :: Text
    , aPrimary  :: Bool
    , aActive   :: Bool
    } deriving (Show)


instance FromJSON Account where
  parseJSON = withObject "account" $ \o -> Account
    <$> o .: "id"
    <*> o .: "name"
    <*> (read <$> o .: "balance")
    <*> o .: "currency"
    <*> o .: "type"
    <*> o .: "primary"
    <*> o .: "active"


data FiatAccount = FiatAccount
    { fAccount            :: Account
    , fDepositInformation :: DepositInformation
    } deriving Show


instance FromJSON FiatAccount where
    parseJSON = withObject "fiat account" $ \o -> FiatAccount
      <$> parseJSON (Object o)
      <*> parseJSON (Object o)


newtype CryptoAccount = CryptoAccount Account
    deriving Show


instance FromJSON CryptoAccount where
  parseJSON = withObject "crypto account" $ \o -> CryptoAccount
    <$> parseJSON (Object o)


data CoinbaseAccount = Fiat FiatAccount | Crypto CryptoAccount
    deriving Show


instance FromJSON CoinbaseAccount where
  parseJSON = withObject "coinbase account" $ \o -> do
      t <- String <$> o .: "type"
      case t of
        "fiat"   -> Fiat <$> parseJSON (Object o)
        "wallet" -> Crypto <$> parseJSON (Object o)
        _        -> fail "Unable to parse coinbase account"
