{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE TypeOperators     #-}

module CoinbasePro.Authenticated
  ( accounts
  , account
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
  , deposits
  , deposit
  , makeDeposit
  ) where

import           Control.Monad                      (void)
import           Data.Aeson                         (encode)
import qualified Data.ByteString.Builder            as BB
import qualified Data.ByteString.Lazy.Char8         as LC8
import           Data.Maybe                         (fromMaybe)
import qualified Data.Set                           as S
import           Data.Text                          (Text, pack)
import           Data.Text.Encoding                 (encodeUtf8)
import           Data.Time.Clock                    (UTCTime)
import           Data.UUID                          (toText)
import           Network.HTTP.Types                 (SimpleQuery,
                                                     SimpleQueryItem,
                                                     encodePathSegments,
                                                     methodDelete, methodGet,
                                                     methodPost, renderQuery,
                                                     simpleQueryToQuery)
import           Servant.Client                     (ClientM)

import qualified CoinbasePro.Authenticated.API      as API
import           CoinbasePro.Authenticated.Accounts (Account, AccountHistory,
                                                     AccountId (..), Fees, Hold,
                                                     TrailingVolume (..))
import           CoinbasePro.Authenticated.Deposit  (Deposit,
                                                     DepositRequest (..),
                                                     DepositResponse,
                                                     PaymentMethodId)
import           CoinbasePro.Authenticated.Fills    (Fill)
import           CoinbasePro.Authenticated.Limits   (Limits)
import           CoinbasePro.Authenticated.Orders   (Order, PlaceOrderBody (..),
                                                     STP, Status (..),
                                                     Statuses (..), TimeInForce,
                                                     statuses)
import           CoinbasePro.Authenticated.Request  (CBAuthT (..), authRequest)
import           CoinbasePro.Request                (RequestPath, emptyBody)


import           CoinbasePro.Types                  (ClientOrderId (..),
                                                     OrderId (..), OrderType,
                                                     Price, ProductId (..),
                                                     ProfileId, Side, Size)

accountsPath :: Text
accountsPath = "accounts"


ordersPath :: Text
ordersPath = "orders"


encodeRequestPath :: [Text] -> RequestPath
encodeRequestPath = LC8.toStrict . BB.toLazyByteString . encodePathSegments


mkSimpleQueryItem :: Show a => Text -> a -> SimpleQueryItem
mkSimpleQueryItem s a = (encodeUtf8 s, encodeUtf8 $ pack (show a))


optionalQuery :: Show a => Text -> Maybe a -> [SimpleQueryItem]
optionalQuery t = maybe [] (return . mkSimpleQueryItem t)


mkProductQuery :: Maybe ProductId -> [SimpleQueryItem]
mkProductQuery = optionalQuery "product_id"


mkOrderIdQuery :: Maybe OrderId -> SimpleQuery
mkOrderIdQuery = optionalQuery "order_id"


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
           -> Size
           -> Price
           -> Bool
           -> Maybe OrderType
           -> Maybe STP
           -> Maybe TimeInForce
           -> CBAuthT ClientM Order
placeOrder clordid prid sd sz price po ot stp tif =
    authRequest methodPost requestPath seBody $ API.placeOrder body
  where
    requestPath = encodeRequestPath [ordersPath]
    body        = PlaceOrderBody clordid prid sd sz price po ot stp tif
    seBody      = LC8.toStrict $ encode body


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
    requestPath = encodeRequestPath ["users", "self", "trailing-volume"]


-- | https://docs.pro.coinbase.com/#get-current-exchange-limits
limits :: CBAuthT ClientM Limits
limits = authRequest methodGet requestPath emptyBody API.limits
  where
    requestPath = encodeRequestPath ["users", "self", "exchange-limits"]


-- | https://docs.pro.coinbase.com/#list-deposits
deposits :: Maybe ProfileId
         -> Maybe UTCTime
         -> Maybe UTCTime
         -> Maybe Int
         -> CBAuthT ClientM [Deposit]
deposits prof before after lm = authRequest methodGet requestPath emptyBody API.deposits
  where
    profQ   = optionalQuery "profile_id" prof
    beforeQ = optionalQuery "before" before
    afterQ  = optionalQuery "after" after
    lmQ     = optionalQuery "limit" lm

    query       = renderQuery True . simpleQueryToQuery $ profQ <> beforeQ <> afterQ <> lmQ
    requestPath = encodeRequestPath ["transfers"] <> query


-- | https://docs.pro.coinbase.com/#single-deposit
deposit :: PaymentMethodId -> CBAuthT ClientM Deposit
deposit d = authRequest methodGet requestPath emptyBody $ API.deposit d
  where
    requestPath = encodeRequestPath ["transfers", pack $ show d]


-- | https://docs.pro.coinbase.com/#payment-method
makeDeposit :: Double
            -> Text
            -> PaymentMethodId
            -> CBAuthT ClientM DepositResponse
makeDeposit amt cur pmi =
    authRequest methodPost requestPath seBody $ API.makeDeposit body
  where
    requestPath = encodeRequestPath ["deposits", "payment-method"]
    body        = DepositRequest amt cur pmi
    seBody      = LC8.toStrict $ encode body
