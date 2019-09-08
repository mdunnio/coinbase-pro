{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}

module CoinbasePro.Request.Authenticated
  ( accounts
  , account
  , listOrders
  , fills
  , placeOrder
  , cancelOrder
  , cancelAll
  ) where


import           Control.Monad                              (void)
import           Control.Monad.IO.Class                     (MonadIO)
import           Data.Aeson                                 (encode)
import qualified Data.ByteString.Char8                      as C8
import qualified Data.ByteString.Lazy.Char8                 as LC8
import           Data.Maybe                                 (fromMaybe)
import           Data.Proxy                                 (Proxy (..))
import qualified Data.Set                                   as S
import           Data.Text                                  (Text, pack,
                                                             toLower, unpack)
import           Network.HTTP.Types                         (SimpleQuery,
                                                             SimpleQueryItem,
                                                             methodDelete,
                                                             methodGet,
                                                             methodPost,
                                                             renderQuery,
                                                             simpleQueryToQuery)
import           Servant.API
import           Servant.Client

import           CoinbasePro.Headers                        (CBAccessKey (..), CBAccessPassphrase (..),
                                                             CBAccessSign (..),
                                                             CBAccessTimeStamp (..),
                                                             RequiredHeader,
                                                             UserAgent,
                                                             UserAgentHeader)
import           CoinbasePro.Request                        (CBAuthT,
                                                             RequestPath,
                                                             authRequest)
import           CoinbasePro.Request.Authenticated.Accounts (Account,
                                                             AccountId (..))
import           CoinbasePro.Request.Authenticated.Fills    (Fill)
import           CoinbasePro.Request.Authenticated.Orders   (Order,
                                                             PlaceOrderBody (..),
                                                             STP, Status (..),
                                                             Statuses (..),
                                                             TimeInForce,
                                                             statuses)
import           CoinbasePro.Types                          (OrderId (..),
                                                             OrderType, Price,
                                                             ProductId (..),
                                                             Side, Size)

type API = "accounts"
    :> RequiredHeader "CB-ACCESS-KEY" CBAccessKey
    :> RequiredHeader "CB-ACCESS-SIGN" CBAccessSign
    :> RequiredHeader "CB-ACCESS-TIMESTAMP" CBAccessTimeStamp
    :> RequiredHeader "CB-ACCESS-PASSPHRASE" CBAccessPassphrase
    :> UserAgentHeader
    :> Get '[JSON] [Account]
    :<|> "accounts" :> Capture "account-id" AccountId
    :> RequiredHeader "CB-ACCESS-KEY" CBAccessKey
    :> RequiredHeader "CB-ACCESS-SIGN" CBAccessSign
    :> RequiredHeader "CB-ACCESS-TIMESTAMP" CBAccessTimeStamp
    :> RequiredHeader "CB-ACCESS-PASSPHRASE" CBAccessPassphrase
    :> UserAgentHeader
    :> Get '[JSON] Account
    :<|> "orders"
    :> QueryParams "status" Status
    :> QueryParam "product_id" ProductId
    :> RequiredHeader "CB-ACCESS-KEY" CBAccessKey
    :> RequiredHeader "CB-ACCESS-SIGN" CBAccessSign
    :> RequiredHeader "CB-ACCESS-TIMESTAMP" CBAccessTimeStamp
    :> RequiredHeader "CB-ACCESS-PASSPHRASE" CBAccessPassphrase
    :> UserAgentHeader
    :> Get '[JSON] [Order]
    :<|> "orders"
    :> ReqBody '[JSON] PlaceOrderBody
    :> RequiredHeader "CB-ACCESS-KEY" CBAccessKey
    :> RequiredHeader "CB-ACCESS-SIGN" CBAccessSign
    :> RequiredHeader "CB-ACCESS-TIMESTAMP" CBAccessTimeStamp
    :> RequiredHeader "CB-ACCESS-PASSPHRASE" CBAccessPassphrase
    :> UserAgentHeader
    :> Post '[JSON] Order
    :<|> "orders" :> Capture "order_id" OrderId
    :> RequiredHeader "CB-ACCESS-KEY" CBAccessKey
    :> RequiredHeader "CB-ACCESS-SIGN" CBAccessSign
    :> RequiredHeader "CB-ACCESS-TIMESTAMP" CBAccessTimeStamp
    :> RequiredHeader "CB-ACCESS-PASSPHRASE" CBAccessPassphrase
    :> UserAgentHeader
    :> Delete '[JSON] NoContent
    :<|> "orders"
    :> QueryParam "product_id" ProductId
    :> RequiredHeader "CB-ACCESS-KEY" CBAccessKey
    :> RequiredHeader "CB-ACCESS-SIGN" CBAccessSign
    :> RequiredHeader "CB-ACCESS-TIMESTAMP" CBAccessTimeStamp
    :> RequiredHeader "CB-ACCESS-PASSPHRASE" CBAccessPassphrase
    :> UserAgentHeader
    :> Delete '[JSON] [OrderId]
    :<|> "fills"
    :> QueryParam "product_id" ProductId
    :> QueryParam "order_id" OrderId
    :> RequiredHeader "CB-ACCESS-KEY" CBAccessKey
    :> RequiredHeader "CB-ACCESS-SIGN" CBAccessSign
    :> RequiredHeader "CB-ACCESS-TIMESTAMP" CBAccessTimeStamp
    :> RequiredHeader "CB-ACCESS-PASSPHRASE" CBAccessPassphrase
    :> UserAgentHeader
    :> Get '[JSON] [Fill]


api :: Proxy API
api = Proxy


accountsAPI :: CBAccessKey -> CBAccessSign -> CBAccessTimeStamp -> CBAccessPassphrase -> UserAgent -> ClientM [Account]
singleAccountAPI :: AccountId -> CBAccessKey -> CBAccessSign -> CBAccessTimeStamp -> CBAccessPassphrase -> UserAgent -> ClientM Account
listOrdersAPI :: [Status] -> Maybe ProductId -> CBAccessKey -> CBAccessSign -> CBAccessTimeStamp -> CBAccessPassphrase -> UserAgent -> ClientM [Order]
placeOrderAPI :: PlaceOrderBody
              -> CBAccessKey -> CBAccessSign -> CBAccessTimeStamp -> CBAccessPassphrase -> UserAgent
              -> ClientM Order
cancelOrderAPI :: OrderId -> CBAccessKey -> CBAccessSign -> CBAccessTimeStamp -> CBAccessPassphrase -> UserAgent -> ClientM NoContent
cancelAllAPI :: Maybe ProductId -> CBAccessKey -> CBAccessSign -> CBAccessTimeStamp -> CBAccessPassphrase -> UserAgent -> ClientM [OrderId]
fillsAPI :: Maybe ProductId -> Maybe OrderId -> CBAccessKey -> CBAccessSign -> CBAccessTimeStamp -> CBAccessPassphrase -> UserAgent -> ClientM [Fill]
accountsAPI :<|> singleAccountAPI :<|> listOrdersAPI :<|> placeOrderAPI :<|> cancelOrderAPI :<|> cancelAllAPI :<|> fillsAPI = client api


-- | https://docs.pro.coinbase.com/?javascript#accounts
accounts :: MonadIO m => CBAuthT m [Account]
accounts = authRequest methodGet "/accounts" "" accountsAPI


-- | https://docs.pro.coinbase.com/?javascript#get-an-account
account :: MonadIO m => AccountId -> CBAuthT m Account
account aid@(AccountId t) = authRequest methodGet requestPath "" $ singleAccountAPI aid
  where
    requestPath = "/accounts/" ++ unpack t


-- | https://docs.pro.coinbase.com/?javascript#list-orders
listOrders :: MonadIO m => Maybe [Status] -> Maybe ProductId -> CBAuthT m [Order]
listOrders st prid = authRequest methodGet (mkRequestPath "/orders") "" $ listOrdersAPI (defaultStatus st) prid
  where
    mkRequestPath :: RequestPath -> RequestPath
    mkRequestPath rp = rp ++ (C8.unpack . renderQuery True . simpleQueryToQuery $ mkOrderQuery st prid)

    mkOrderQuery :: Maybe [Status] -> Maybe ProductId -> SimpleQuery
    mkOrderQuery ss p = mkStatusQuery ss <> mkProductQuery p

    mkStatusQuery :: Maybe [Status] -> [SimpleQueryItem]
    mkStatusQuery ss = mkSimpleQueryItem "status" . toLower . pack . show <$> S.toList (unStatuses . statuses $ defaultStatus ss)

    defaultStatus :: Maybe [Status] -> [Status]
    defaultStatus = fromMaybe [All]


-- | https://docs.pro.coinbase.com/?javascript#place-a-new-order
placeOrder :: MonadIO m => ProductId -> Side -> Size -> Price -> Bool -> Maybe OrderType -> Maybe STP -> Maybe TimeInForce -> CBAuthT m Order
placeOrder prid sd sz price po ot stp tif =
    authRequest methodPost "/orders" (LC8.unpack $ encode body) $ placeOrderAPI body
  where
    body = PlaceOrderBody prid sd sz price po ot stp tif


-- | https://docs.pro.coinbase.com/?javascript#cancel-an-order
cancelOrder :: MonadIO m => OrderId -> CBAuthT m ()
cancelOrder oid = void . authRequest methodDelete (mkRequestPath "/orders") "" $ cancelOrderAPI oid
  where
    mkRequestPath :: RequestPath -> RequestPath
    mkRequestPath rp = rp ++ "/" ++ unpack (unOrderId oid)


-- | https://docs.pro.coinbase.com/?javascript#cancel-all
cancelAll :: MonadIO m => Maybe ProductId -> CBAuthT m [OrderId]
cancelAll prid = authRequest methodDelete (mkRequestPath "/orders") "" (cancelAllAPI prid)
  where
    mkRequestPath :: RequestPath -> RequestPath
    mkRequestPath rp = rp ++ (C8.unpack . renderQuery True . simpleQueryToQuery $ mkProductQuery prid)


-- | https://docs.pro.coinbase.com/?javascript#fills
fills :: MonadIO m => Maybe ProductId -> Maybe OrderId -> CBAuthT m [Fill]
fills prid oid = authRequest methodGet mkRequestPath "" (fillsAPI prid oid)
  where
    brp = "/fills"

    mkRequestPath :: RequestPath
    mkRequestPath = brp ++ (C8.unpack . renderQuery True . simpleQueryToQuery $ mkSimpleQuery prid oid)

    mkSimpleQuery :: Maybe ProductId -> Maybe OrderId -> SimpleQuery
    mkSimpleQuery p o = mkProductQuery p <> mkOrderIdQuery o


mkSimpleQueryItem :: String -> Text -> SimpleQueryItem
mkSimpleQueryItem s t = (C8.pack s, C8.pack $ unpack t)


mkProductQuery :: Maybe ProductId -> [SimpleQueryItem]
mkProductQuery = maybe [] (return . mkSimpleQueryItem "product_id" . unProductId)


mkOrderIdQuery :: Maybe OrderId -> SimpleQuery
mkOrderIdQuery = maybe [] (return . mkSimpleQueryItem "order_id" . unOrderId)
