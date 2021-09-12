{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}

module CoinbasePro.Authenticated
  ( accounts
  , account
  , accountHistory
  , accountHolds
  , listOrders
  , getOrder
  , getClientOrder
  , placeOrder
  , placeOrderBody
  , cancelOrder
  , cancelAll
  , fills
  , fees
  , trailingVolume
  , limits
  , deposits
  , withdrawals
  , transfer
  , makeDeposit
  , makeCoinbaseDeposit
  , cryptoDepositAddress
  , makeWithdrawal
  , makeCoinbaseWithdrawal
  , makeCryptoWithdrawal
  , withdrawalFeeEstimate
  , createStablecoinConversion
  , paymentMethods
  , coinbaseAccounts
  , profiles
  , profile
  , profileTransfer
  , createReport
  , getReport
  , getOracle
  ) where

import           Control.Monad                              (void)
import           Data.Aeson                                 (ToJSON, encode)
import           Data.ByteString                            (ByteString)
import qualified Data.ByteString.Lazy.Char8                 as LC8
import           Data.Maybe                                 (fromMaybe)
import qualified Data.Set                                   as S
import           Data.Text                                  (Text, pack)
import           Data.Text.Encoding                         (encodeUtf8)
import           Data.Time.Clock                            (UTCTime)
import           Data.UUID                                  (toText)
import           Network.HTTP.Types                         (SimpleQuery,
                                                             SimpleQueryItem,
                                                             methodDelete,
                                                             methodGet,
                                                             methodPost,
                                                             renderQuery,
                                                             simpleQueryToQuery)
import           Servant.Client                             (ClientM)

import qualified CoinbasePro.Authenticated.API              as API
import           CoinbasePro.Authenticated.Accounts         (Account,
                                                             AccountHistory,
                                                             AccountId (..),
                                                             Fees, Hold,
                                                             TrailingVolume (..))
import           CoinbasePro.Authenticated.CoinbaseAccounts (CoinbaseAccount)
import           CoinbasePro.Authenticated.Conversion       (StablecoinConversionRequest (..),
                                                             StablecoinConversionResponse)
import           CoinbasePro.Authenticated.Deposit          (CoinbaseDepositRequest (..),
                                                             CryptoDepositAddress,
                                                             DepositRequest (..),
                                                             DepositResponse)
import           CoinbasePro.Authenticated.Fills            (Fill)
import           CoinbasePro.Authenticated.Limits           (Limits)
import           CoinbasePro.Authenticated.Oracle           (OracleResponse)
import           CoinbasePro.Authenticated.Orders           (Order,
                                                             PlaceOrderBody (..),
                                                             STP, Status (..),
                                                             Statuses (..),
                                                             TimeInForce,
                                                             statuses)
import           CoinbasePro.Authenticated.Payment          (PaymentMethod,
                                                             PaymentMethodId (..))
import           CoinbasePro.Authenticated.Profile          (Profile,
                                                             ProfileTransfer (..))
import           CoinbasePro.Authenticated.Report           (ReportId (..),
                                                             ReportRequest (..),
                                                             ReportResponse)
import           CoinbasePro.Authenticated.Request          (CBAuthT (..),
                                                             authRequest)
import           CoinbasePro.Authenticated.Transfer         (Transfer,
                                                             TransferType (..))
import           CoinbasePro.Authenticated.Withdrawal       (CoinbaseWithdrawalRequest (..),
                                                             CryptoWithdrawalRequest (..),
                                                             CryptoWithdrawalResponse,
                                                             WithdrawalFeeEstimateResponse (..),
                                                             WithdrawalRequest (..),
                                                             WithdrawalResponse)
import           CoinbasePro.Request                        (emptyBody,
                                                             encodeRequestPath)


import           CoinbasePro.Types                          (ClientOrderId (..),
                                                             CryptoAddress (..),
                                                             CurrencyType (..),
                                                             OrderId (..),
                                                             OrderType, Price,
                                                             ProductId (..),
                                                             ProfileId, Side,
                                                             Size)


accountsPath :: Text
accountsPath = "accounts"


ordersPath :: Text
ordersPath = "orders"


depositsPath :: Text
depositsPath = "deposits"


withdrawalsPath :: Text
withdrawalsPath = "withdrawals"


coinbaseAccountPath :: Text
coinbaseAccountPath = "coinbase-account"


profilesPath :: Text
profilesPath = "profiles"


transferPath :: Text
transferPath = "transfer"


usersPath :: Text
usersPath = "users"


selfPath :: Text
selfPath = "self"


reportsPath :: Text
reportsPath = "reports"


mkSimpleQueryItem :: Show a => Text -> a -> SimpleQueryItem
mkSimpleQueryItem s a = (encodeUtf8 s, encodeUtf8 $ pack (show a))


optionalQuery :: Show a => Text -> Maybe a -> [SimpleQueryItem]
optionalQuery t = maybe [] (return . mkSimpleQueryItem t)


mkProductQuery :: Maybe ProductId -> [SimpleQueryItem]
mkProductQuery = optionalQuery "product_id"


mkOrderIdQuery :: Maybe OrderId -> SimpleQuery
mkOrderIdQuery = optionalQuery "order_id"


encodeBody :: (ToJSON b) => b -> ByteString
encodeBody = LC8.toStrict . encode


-- | https://docs.pro.coinbase.com/#accounts
accounts :: CBAuthT ClientM [Account]
accounts = authRequest methodGet requestPath emptyBody API.accounts
  where
    requestPath = encodeRequestPath [accountsPath]


-- | https://docs.pro.coinbase.com/#get-an-account
account :: AccountId -> CBAuthT ClientM Account
account aid@(AccountId t) = authRequest methodGet requestPath emptyBody $ API.singleAccount aid
  where
    requestPath = encodeRequestPath [accountsPath, t]


-- | https://docs.pro.coinbase.com/#get-account-history
accountHistory :: AccountId -> CBAuthT ClientM [AccountHistory]
accountHistory aid@(AccountId t) = authRequest methodGet requestPath emptyBody $ API.accountHistory aid
  where
    ledgerPath  = "ledger"
    requestPath = encodeRequestPath [accountsPath, t, ledgerPath]


-- | https://docs.pro.coinbase.com/#get-holds
accountHolds :: AccountId -> CBAuthT ClientM [Hold]
accountHolds aid@(AccountId t) = authRequest methodGet requestPath emptyBody $ API.accountHolds aid
  where
    holdsPath   = "holds"
    requestPath = encodeRequestPath [accountsPath, t, holdsPath]


-- | https://docs.pro.coinbase.com/#list-orders
listOrders :: Maybe [Status] -> Maybe ProductId -> CBAuthT ClientM [Order]
listOrders st prid = authRequest methodGet requestPath emptyBody $ API.listOrders (defaultStatus st) prid
  where
    query       = renderQuery True . simpleQueryToQuery $ orderQuery st prid
    requestPath = encodeRequestPath [ordersPath] <> query

    orderQuery :: Maybe [Status] -> Maybe ProductId -> SimpleQuery
    orderQuery ss p = statusQuery ss <> mkProductQuery p

    statusQuery :: Maybe [Status] -> [SimpleQueryItem]
    statusQuery ss = mkSimpleQueryItem "status" <$> S.toList (unStatuses . statuses $ defaultStatus ss)

    defaultStatus :: Maybe [Status] -> [Status]
    defaultStatus = fromMaybe [All]


-- | https://docs.pro.coinbase.com/#get-an-order
getOrder :: OrderId -> CBAuthT ClientM Order
getOrder oid = authRequest methodGet requestPath emptyBody $ API.getOrder oid
  where
    requestPath = encodeRequestPath [ordersPath, unOrderId oid]


-- | https://docs.pro.coinbase.com/#get-an-order
getClientOrder :: ClientOrderId -> CBAuthT ClientM Order
getClientOrder cloid = authRequest methodGet requestPath emptyBody $ API.getClientOrder cloid
  where
    oid         = toText $ unClientOrderId cloid
    requestPath = encodeRequestPath [ordersPath, "client:" <> oid]


-- | https://docs.pro.coinbase.com/#place-a-new-order
placeOrder :: Maybe ClientOrderId
              -> ProductId
              -> Side
              -> Maybe Size
              -> Maybe Price
              -> Maybe Bool
              -> Maybe OrderType
              -> Maybe STP
              -> Maybe TimeInForce
              -> CBAuthT ClientM Order
placeOrder clordid prid sd sz price po ot stp tif =
    authRequest methodPost requestPath (encodeBody body) $ API.placeOrder body
  where
    requestPath = encodeRequestPath [ordersPath]
    body        = PlaceOrderBody clordid prid sd sz Nothing price po ot stp tif Nothing Nothing


-- | https://docs.pro.coinbase.com/#place-a-new-order
-- Use the underlying post body record
placeOrderBody :: PlaceOrderBody -> CBAuthT ClientM Order
placeOrderBody body =
    authRequest methodPost requestPath (encodeBody body) $ API.placeOrder body
  where
    requestPath = encodeRequestPath [ordersPath]


-- | https://docs.pro.coinbase.com/#cancel-an-order
cancelOrder :: OrderId -> CBAuthT ClientM ()
cancelOrder oid = void . authRequest methodDelete requestPath emptyBody $ API.cancelOrder oid
  where
    requestPath = encodeRequestPath [ordersPath, unOrderId oid]


-- | https://docs.pro.coinbase.com/#cancel-all
cancelAll :: Maybe ProductId -> CBAuthT ClientM [OrderId]
cancelAll prid = authRequest methodDelete requestPath emptyBody (API.cancelAll prid)
  where
    query       = renderQuery True . simpleQueryToQuery $ mkProductQuery prid
    requestPath = encodeRequestPath [ordersPath] <> query


-- | https://docs.pro.coinbase.com/#fills
fills :: Maybe ProductId -> Maybe OrderId -> CBAuthT ClientM [Fill]
fills prid oid = authRequest methodGet requestPath emptyBody (API.fills prid oid)
  where
    fillsPath   = "fills"
    query       = renderQuery True . simpleQueryToQuery $ mkSimpleQuery prid oid
    requestPath = encodeRequestPath [fillsPath] <> query

    mkSimpleQuery :: Maybe ProductId -> Maybe OrderId -> SimpleQuery
    mkSimpleQuery p o = mkProductQuery p <> mkOrderIdQuery o


-- | https://docs.pro.coinbase.com/#get-current-fees
fees :: CBAuthT ClientM Fees
fees = authRequest methodGet feesRequestPath emptyBody API.fees
  where
    feesPath        = "fees"
    feesRequestPath = encodeRequestPath [feesPath]


-- | https://docs.pro.coinbase.com/#trailing-volume
trailingVolume :: CBAuthT ClientM [TrailingVolume]
trailingVolume = authRequest methodGet requestPath emptyBody API.trailingVolume
  where
    requestPath = encodeRequestPath [usersPath, selfPath, "trailing-volume"]


-- | https://docs.pro.coinbase.com/#get-current-exchange-limits
limits :: CBAuthT ClientM Limits
limits = authRequest methodGet requestPath emptyBody API.limits
  where
    requestPath = encodeRequestPath [usersPath, selfPath, "exchange-limits"]


-- | https://docs.pro.coinbase.com/#list-deposits
deposits :: Maybe ProfileId
         -> Maybe UTCTime
         -> Maybe UTCTime
         -> Maybe Int
         -> CBAuthT ClientM [Transfer]
deposits = transfers DepositTransferType


-- | https://docs.pro.coinbase.com/#list-withdrawals
withdrawals :: Maybe ProfileId
            -> Maybe UTCTime
            -> Maybe UTCTime
            -> Maybe Int
            -> CBAuthT ClientM [Transfer]
withdrawals = transfers WithdrawTransferType


transfers :: TransferType
          -> Maybe ProfileId
          -> Maybe UTCTime
          -> Maybe UTCTime
          -> Maybe Int
          -> CBAuthT ClientM [Transfer]
transfers tt prof before after lm = authRequest methodGet requestPath emptyBody $
    API.transfers tt prof before after lm
  where
    typeQ   = return $ mkSimpleQueryItem "type" tt
    profQ   = optionalQuery "profile_id" prof
    beforeQ = optionalQuery "before" before
    afterQ  = optionalQuery "after" after
    lmQ     = optionalQuery "limit" lm

    query       = renderQuery True . simpleQueryToQuery $ typeQ <> profQ <> beforeQ <> afterQ <> lmQ
    requestPath = encodeRequestPath [transferPath <> "s"] <> query


-- | https://docs.pro.coinbase.com/#single-deposit
-- | https://docs.pro.coinbase.com/#single-withdrawal
transfer :: PaymentMethodId -> CBAuthT ClientM Transfer
transfer pmt@(PaymentMethodId p) = authRequest methodGet requestPath emptyBody $ API.transfer pmt
  where
    requestPath = encodeRequestPath [transferPath <> "s", p]


-- | https://docs.pro.coinbase.com/#payment-method
makeDeposit :: Double
            -> Text
            -> PaymentMethodId
            -> CBAuthT ClientM DepositResponse
makeDeposit amt cur pmi =
    authRequest methodPost requestPath (encodeBody body) $ API.makeDeposit body
  where
    requestPath = encodeRequestPath [depositsPath, "payment-method"]
    body        = DepositRequest amt cur pmi


-- | https://docs.pro.coinbase.com/#coinbase
makeCoinbaseDeposit :: Double
                    -> Text
                    -> AccountId
                    -> CBAuthT ClientM DepositResponse
makeCoinbaseDeposit amt cur act =
    authRequest methodPost requestPath (encodeBody body) $ API.makeCoinbaseDeposit body
  where
    requestPath = encodeRequestPath [depositsPath, coinbaseAccountPath]
    body        = CoinbaseDepositRequest amt cur act


-- | https://docs.pro.coinbase.com/#generate-a-crypto-deposit-address
cryptoDepositAddress :: AccountId -> CBAuthT ClientM CryptoDepositAddress
cryptoDepositAddress act =
    authRequest methodPost requestPath emptyBody $ API.cryptoDepositAddress act
  where
    requestPath = encodeRequestPath [coinbaseAccountPath <> "s", pack $ show act, "addresses"]


-- | https://docs.pro.coinbase.com/#payment-method55
makeWithdrawal :: Double
               -> Text
               -> PaymentMethodId
               -> CBAuthT ClientM WithdrawalResponse
makeWithdrawal amt cur pmi =
    authRequest methodPost requestPath (encodeBody body) $ API.makeWithdrawal body
  where
    requestPath = encodeRequestPath [withdrawalsPath, "payment-method"]
    body        = WithdrawalRequest amt cur pmi


-- | https://docs.pro.coinbase.com/#coinbase56
makeCoinbaseWithdrawal :: Double
                       -> Text
                       -> AccountId
                       -> CBAuthT ClientM WithdrawalResponse
makeCoinbaseWithdrawal amt cur act =
    authRequest methodPost requestPath (encodeBody body) $ API.makeCoinbaseWithdrawal body
  where
    requestPath = encodeRequestPath [withdrawalsPath, coinbaseAccountPath]
    body        = CoinbaseWithdrawalRequest amt cur act


-- | https://docs.pro.coinbase.com/#crypto
makeCryptoWithdrawal :: Double
                     -> Text
                     -> Text
                     -> CBAuthT ClientM CryptoWithdrawalResponse
makeCryptoWithdrawal amt cur addr =
    authRequest methodPost requestPath (encodeBody body) $ API.makeCryptoWithdrawal body
  where
    requestPath = encodeRequestPath [withdrawalsPath, "crypto"]
    body        = CryptoWithdrawalRequest amt cur addr


-- | https://docs.pro.coinbase.com/#fee-estimate
withdrawalFeeEstimate :: CurrencyType
                      -> CryptoAddress
                      -> CBAuthT ClientM WithdrawalFeeEstimateResponse
withdrawalFeeEstimate cur addr =
    authRequest methodGet requestPath emptyBody $ API.withdrawalFeeEstimate cur addr
  where
    curQ  = return $ mkSimpleQueryItem "currency" cur
    addrQ = return $ mkSimpleQueryItem "crypto_address" addr

    query       = renderQuery True . simpleQueryToQuery $ curQ <> addrQ
    requestPath = encodeRequestPath [withdrawalsPath, "fee-estimate"] <> query


-- | https://docs.pro.coinbase.com/#stablecoin-conversions
createStablecoinConversion :: CurrencyType -> CurrencyType -> Double -> CBAuthT ClientM StablecoinConversionResponse
createStablecoinConversion fromCur toCur amt =
    authRequest methodPost requestPath (encodeBody body) $ API.createStablecoinConversion body
  where
    requestPath = encodeRequestPath ["conversions"]
    body        = StablecoinConversionRequest fromCur toCur amt


-- | https://docs.pro.coinbase.com/#list-payment-methods
paymentMethods :: CBAuthT ClientM [PaymentMethod]
paymentMethods = authRequest methodGet requestPath emptyBody API.paymentMethods
  where
    requestPath = encodeRequestPath ["payment-methods"]


-- | https://docs.pro.coinbase.com/#list-accounts64
coinbaseAccounts :: CBAuthT ClientM [CoinbaseAccount]
coinbaseAccounts = authRequest methodGet requestPath emptyBody API.coinbaseAccounts
  where
    requestPath = encodeRequestPath [coinbaseAccountPath <> "s"]


-- | https://docs.pro.coinbase.com/#list-profiles
profiles :: Maybe Bool -> CBAuthT ClientM [Profile]
profiles active = authRequest methodGet requestPath emptyBody $ API.profiles active
  where
    activeQ  = optionalQuery "active" active

    query       = renderQuery True . simpleQueryToQuery $ activeQ
    requestPath = encodeRequestPath [profilesPath] <> query


-- | https://docs.pro.coinbase.com/#get-a-profile
profile :: ProfileId -> CBAuthT ClientM Profile
profile profId = authRequest methodGet requestPath emptyBody $ API.profile profId
  where
    requestPath = encodeRequestPath [profilesPath, profId]


-- | https://docs.pro.coinbase.com/#create-profile-transfer
profileTransfer :: ProfileId
                -> ProfileId
                -> CurrencyType
                -> String
                -> CBAuthT ClientM ()
profileTransfer fromProf toProf cur amt = void . authRequest methodPost requestPath (encodeBody body) $ API.profileTransfer body
  where
    requestPath = encodeRequestPath [profilesPath, transferPath]
    body        = ProfileTransfer fromProf toProf cur amt


-- | https://docs.pro.coinbase.com/#create-a-new-report
createReport :: ReportRequest -> CBAuthT ClientM ReportResponse
createReport req = authRequest methodPost requestPath (encodeBody req) $ API.createReport req
  where
    requestPath = encodeRequestPath [reportsPath]


-- | https://docs.pro.coinbase.com/#get-report-status
getReport :: ReportId -> CBAuthT ClientM ReportResponse
getReport rid = authRequest methodGet requestPath emptyBody $ API.getReport rid
  where
    requestPath = encodeRequestPath [reportsPath, toText $ unReportId rid]


-- | https://docs.pro.coinbase.com/#oracle
getOracle :: CBAuthT ClientM OracleResponse
getOracle = authRequest methodGet requestPath emptyBody API.getOracle
  where
    requestPath = encodeRequestPath ["oracle"]
