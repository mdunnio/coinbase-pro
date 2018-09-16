{-# LANGUAGE OverloadedStrings #-}

module CoinbasePro.Accounts
    ( getAccounts
    ) where

import           Data.Text                         (Text)

import           CoinbasePro.Request.Authenticated (CBAuthT, authRequest)


getAccounts :: CBAuthT IO (Maybe Text)
getAccounts = authRequest "/accounts" ""
