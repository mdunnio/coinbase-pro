{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeFamilies  #-}
{-# LANGUAGE TypeOperators #-}

module CoinbasePro.Authenticated.API
    ( accounts
    , singleAccount
    , accountHistory
    , accountHolds
    , listOrders
    , getOrder
    , getClientOrder
    , placeOrder
    , cancelOrder
    , cancelAll
    , fills
    , fees
    , trailingVolume
    , limits
    ) where

import           Data.Proxy                         (Proxy (..))
import           Servant.API                        (AuthProtect, Capture, JSON,
                                                     NoContent, QueryParam,
                                                     QueryParams, ReqBody,
                                                     (:<|>) (..), (:>))
import           Servant.Client
import           Servant.Client.Core                (AuthenticatedRequest)

import           CoinbasePro.Authenticated.Accounts (Account, AccountHistory,
                                                     AccountId (..), Fees, Hold,
                                                     TrailingVolume)
import           CoinbasePro.Authenticated.Fills    (Fill)
import           CoinbasePro.Authenticated.Limits   (Limits)
import           CoinbasePro.Authenticated.Orders   (Order, PlaceOrderBody (..),
                                                     Status (..))
import           CoinbasePro.Authenticated.Request  (AuthDelete, AuthGet,
                                                     AuthPost)
import           CoinbasePro.Types                  (ClientOrderId (..),
                                                     OrderId (..),
                                                     ProductId (..))


type API =    "accounts" :> AuthGet [Account]
         :<|> "accounts" :> Capture "account-id" AccountId :> AuthGet Account
         :<|> "accounts" :> Capture "account_id" AccountId :> "ledger" :> AuthGet [AccountHistory]
         :<|> "accounts" :> Capture "account_id" AccountId :> "holds" :> AuthGet [Hold]
         :<|> "orders" :> QueryParams "status" Status :> QueryParam "product_id" ProductId :> AuthGet [Order]
         :<|> "orders" :> Capture "order_id" OrderId :> AuthGet Order
         :<|> "orders" :> Capture "client_oid" ClientOrderId :> AuthGet Order
         :<|> "orders" :> ReqBody '[JSON] PlaceOrderBody :> AuthPost Order
         :<|> "orders" :> Capture "order_id" OrderId :> AuthDelete NoContent
         :<|> "orders" :> QueryParam "product_id" ProductId :> AuthDelete [OrderId]
         :<|> "fills" :> QueryParam "product_id" ProductId :> QueryParam "order_id" OrderId :> AuthGet [Fill]
         :<|> "fees" :> AuthGet Fees
         :<|> "users" :> "self" :> "trailing-volume" :> AuthGet [TrailingVolume]
         :<|> "users" :> "self" :> "exchange-limits" :> AuthGet Limits


api :: Proxy API
api = Proxy


accounts :: AuthenticatedRequest (AuthProtect "CBAuth") -> ClientM [Account]
singleAccount :: AccountId -> AuthenticatedRequest (AuthProtect "CBAuth") -> ClientM Account
accountHistory :: AccountId -> AuthenticatedRequest (AuthProtect "CBAuth") -> ClientM [AccountHistory]
accountHolds :: AccountId -> AuthenticatedRequest (AuthProtect "CBAuth") -> ClientM [Hold]
listOrders :: [Status] -> Maybe ProductId -> AuthenticatedRequest (AuthProtect "CBAuth") -> ClientM [Order]
getOrder :: OrderId -> AuthenticatedRequest (AuthProtect "CBAuth") -> ClientM Order
getClientOrder :: ClientOrderId -> AuthenticatedRequest (AuthProtect "CBAuth") -> ClientM Order
placeOrder :: PlaceOrderBody -> AuthenticatedRequest (AuthProtect "CBAuth") -> ClientM Order
cancelOrder :: OrderId -> AuthenticatedRequest (AuthProtect "CBAuth") -> ClientM NoContent
cancelAll :: Maybe ProductId -> AuthenticatedRequest (AuthProtect "CBAuth") -> ClientM [OrderId]
fills :: Maybe ProductId -> Maybe OrderId -> AuthenticatedRequest (AuthProtect "CBAuth") -> ClientM [Fill]
fees :: AuthenticatedRequest (AuthProtect "CBAuth") -> ClientM Fees
trailingVolume :: AuthenticatedRequest (AuthProtect "CBAuth") -> ClientM [TrailingVolume]
limits :: AuthenticatedRequest (AuthProtect "CBAuth") -> ClientM Limits
accounts :<|> singleAccount :<|> accountHistory :<|> accountHolds :<|> listOrders :<|> getOrder :<|> getClientOrder :<|> placeOrder :<|> cancelOrder :<|> cancelAll :<|> fills :<|> fees :<|> trailingVolume :<|> limits = client api
