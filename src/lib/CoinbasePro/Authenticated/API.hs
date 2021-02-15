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
    , transfers
    , transfer
    , makeDeposit
    , makeCoinbaseDeposit
    , cryptoDepositAddress
    , makeWithdrawal
    , makeCoinbaseWithdrawal
    , makeCryptoWithdrawal
    , withdrawalFeeEstimate
    , paymentMethods
    , coinbaseAccounts
    ) where

import           Data.Proxy                                 (Proxy (..))
import           Data.Time.Clock                            (UTCTime)
import           Servant.API                                (AuthProtect,
                                                             Capture, JSON,
                                                             NoContent,
                                                             QueryParam,
                                                             QueryParam',
                                                             QueryParams,
                                                             ReqBody, Required,
                                                             (:<|>) (..), (:>))
import           Servant.Client
import           Servant.Client.Core                        (AuthenticatedRequest)

import           CoinbasePro.Authenticated.Accounts         (Account,
                                                             AccountHistory,
                                                             AccountId (..),
                                                             Fees, Hold,
                                                             TrailingVolume)
import           CoinbasePro.Authenticated.CoinbaseAccounts (CoinbaseAccount)
import           CoinbasePro.Authenticated.Deposit          (CoinbaseDepositRequest,
                                                             CryptoDepositAddress,
                                                             DepositRequest,
                                                             DepositResponse)
import           CoinbasePro.Authenticated.Fills            (Fill)
import           CoinbasePro.Authenticated.Limits           (Limits)
import           CoinbasePro.Authenticated.Orders           (Order,
                                                             PlaceOrderBody (..),
                                                             Status (..))
import           CoinbasePro.Authenticated.Payment          (PaymentMethod (..),
                                                             PaymentMethodId)
import           CoinbasePro.Authenticated.Request          (AuthDelete,
                                                             AuthGet, AuthPost)
import           CoinbasePro.Authenticated.Transfer         (Transfer,
                                                             TransferType)
import           CoinbasePro.Authenticated.Withdrawal       (CoinbaseWithdrawalRequest (..),
                                                             CryptoWithdrawalRequest,
                                                             CryptoWithdrawalResponse,
                                                             WithdrawalFeeEstimateResponse,
                                                             WithdrawalRequest,
                                                             WithdrawalResponse)
import           CoinbasePro.Types                          (ClientOrderId (..),
                                                             CryptoAddress,
                                                             CurrencyType,
                                                             OrderId (..),
                                                             ProductId (..),
                                                             ProfileId)


type API =    "accounts" :> AuthGet [Account]
         :<|> "accounts" :> Capture "account_id" AccountId :> AuthGet Account
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
         :<|> "transfers"
             :> QueryParam' '[Required] "type" TransferType
             :> QueryParam "profile_id" ProfileId
             :> QueryParam "before" UTCTime
             :> QueryParam "after" UTCTime
             :> QueryParam "limit" Int
             :> AuthGet [Transfer]
         :<|> "transfers" :> Capture "transfer_id" PaymentMethodId :> AuthGet Transfer
         :<|> "deposits" :> "payment-method" :> ReqBody '[JSON] DepositRequest :> AuthPost DepositResponse
         :<|> "deposits" :> "coinbase-account" :> ReqBody '[JSON] CoinbaseDepositRequest :> AuthPost DepositResponse
         :<|> "coinbase-accounts" :> Capture "account_id" AccountId :> "addresses" :> AuthPost CryptoDepositAddress
         :<|> "withdrawals" :> "payment-method" :> ReqBody '[JSON] WithdrawalRequest :> AuthPost WithdrawalResponse
         :<|> "withdrawals" :> "coinbase-account" :> ReqBody '[JSON] CoinbaseWithdrawalRequest :> AuthPost WithdrawalResponse
         :<|> "withdrawals" :> "crypto" :> ReqBody '[JSON] CryptoWithdrawalRequest :> AuthPost CryptoWithdrawalResponse
         :<|> "withdrawals" :> "fee-estimate"
             :> QueryParam' '[Required] "currency" CurrencyType
             :> QueryParam' '[Required] "crypto_address" CryptoAddress
             :> AuthGet WithdrawalFeeEstimateResponse
         :<|> "payment-methods" :> AuthGet [PaymentMethod]
         :<|> "coinbase-accounts" :> AuthGet [CoinbaseAccount]


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
transfers :: TransferType -> Maybe ProfileId -> Maybe UTCTime -> Maybe UTCTime -> Maybe Int -> AuthenticatedRequest (AuthProtect "CBAuth") -> ClientM [Transfer]
transfer :: PaymentMethodId -> AuthenticatedRequest (AuthProtect "CBAuth") -> ClientM Transfer
makeDeposit :: DepositRequest -> AuthenticatedRequest (AuthProtect "CBAuth") -> ClientM DepositResponse
makeCoinbaseDeposit :: CoinbaseDepositRequest -> AuthenticatedRequest (AuthProtect "CBAuth") -> ClientM DepositResponse
cryptoDepositAddress :: AccountId -> AuthenticatedRequest (AuthProtect "CBAuth") -> ClientM CryptoDepositAddress
makeWithdrawal :: WithdrawalRequest -> AuthenticatedRequest (AuthProtect "CBAuth") -> ClientM WithdrawalResponse
makeCoinbaseWithdrawal :: CoinbaseWithdrawalRequest -> AuthenticatedRequest (AuthProtect "CBAuth") -> ClientM WithdrawalResponse
makeCryptoWithdrawal :: CryptoWithdrawalRequest -> AuthenticatedRequest (AuthProtect "CBAuth") -> ClientM CryptoWithdrawalResponse
withdrawalFeeEstimate :: CurrencyType -> CryptoAddress -> AuthenticatedRequest (AuthProtect "CBAuth") -> ClientM WithdrawalFeeEstimateResponse
paymentMethods :: AuthenticatedRequest (AuthProtect "CBAuth") -> ClientM [PaymentMethod]
coinbaseAccounts :: AuthenticatedRequest (AuthProtect "CBAuth") -> ClientM [CoinbaseAccount]
accounts
  :<|> singleAccount :<|> accountHistory :<|> accountHolds :<|> listOrders :<|> getOrder :<|> getClientOrder :<|> placeOrder
  :<|> cancelOrder :<|> cancelAll :<|> fills :<|> fees :<|> trailingVolume :<|> limits :<|> transfers :<|> transfer
  :<|> makeDeposit :<|> makeCoinbaseDeposit :<|> cryptoDepositAddress :<|> makeWithdrawal :<|> makeCoinbaseWithdrawal
  :<|> makeCryptoWithdrawal :<|> withdrawalFeeEstimate :<|> paymentMethods :<|> coinbaseAccounts = client api
