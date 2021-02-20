{-# LANGUAGE TemplateHaskell #-}

module CoinbasePro.Authenticated.Profile
  ( Profile (..)
  , ProfileTransfer (..)
  ) where

import           Data.Aeson.Casing (snakeCase)
import           Data.Aeson.TH     (defaultOptions, deriveJSON,
                                    fieldLabelModifier)
import           Data.Text         (Text)

import           CoinbasePro.Types (CreatedAt, CurrencyType, ProfileId, UserId)

data Profile = Profile
    { pId        :: ProfileId
    , pUserId    :: UserId
    , pName      :: Text
    , pActive    :: Bool
    , pIsDefault :: Bool
    , pCreatedAt :: CreatedAt
    } deriving Show


deriveJSON defaultOptions { fieldLabelModifier = snakeCase . drop 1 } ''Profile


data ProfileTransfer = ProfileTransfer
    { from     :: ProfileId
    , to       :: ProfileId
    , currency :: CurrencyType
    , amount   :: String
    } deriving Show


deriveJSON defaultOptions { fieldLabelModifier = snakeCase } ''ProfileTransfer
