{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeFamilies  #-}
{-# LANGUAGE TypeOperators #-}

module CoinbasePro.Authenticated
  ( accounts
  , account
  , listOrders
  , placeOrder
  , cancelOrder
  , cancelAll
  , fills
  , trailingVolume
  ) where


import           Control.Monad                      (void)
import           Data.Aeson                         (encode)
import qualified Data.ByteString.Char8              as C8
import qualified Data.ByteString.Lazy.Char8         as LC8
import           Data.Maybe                         (fromMaybe)
import qualified Data.Set                           as S
import           Data.Text                          (Text, pack, toLower,
                                                     unpack)
import           Network.HTTP.Types                 (SimpleQuery,
                                                     SimpleQueryItem,
                                                     methodDelete, methodGet,
                                                     methodPost, renderQuery,
                                                     simpleQueryToQuery)
import           Servant.Client                     (ClientM)

import           CoinbasePro.Authenticated.Accounts (Account, AccountId (..),
                                                     TrailingVolume (..))
import qualified CoinbasePro.Authenticated.API      as API
import           CoinbasePro.Authenticated.Fills    (Fill)
import           CoinbasePro.Authenticated.Orders   (Order, PlaceOrderBody (..),
                                                     STP, Status (..),
                                                     Statuses (..), TimeInForce,
                                                     statuses)
import           CoinbasePro.Authenticated.Request  (CBAuthT (..), authRequest)
import           CoinbasePro.Request                (RequestPath)


import           CoinbasePro.Types                  (OrderId (..), OrderType,
                                                     Price, ProductId (..),
                                                     Side, Size)


-- | https://docs.pro.coinbase.com/?javascript#accounts
accounts :: CBAuthT ClientM [Account]
accounts = authRequest methodGet "/accounts" "" API.accounts


-- | https://docs.pro.coinbase.com/?javascript#get-an-account
account :: AccountId -> CBAuthT ClientM Account
account aid@(AccountId t) = authRequest methodGet requestPath "" $ API.singleAccount aid
  where
    requestPath = "/accounts/" ++ unpack t


-- | https://docs.pro.coinbase.com/?javascript#list-orders
listOrders :: Maybe [Status] -> Maybe ProductId -> CBAuthT ClientM [Order]
listOrders st prid = authRequest methodGet (mkRequestPath "/orders") "" $ API.listOrders (defaultStatus st) prid
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
placeOrder :: ProductId -> Side -> Size -> Price -> Bool -> Maybe OrderType -> Maybe STP -> Maybe TimeInForce -> CBAuthT ClientM Order
placeOrder prid sd sz price po ot stp tif =
    authRequest methodPost "/orders" (LC8.unpack $ encode body) $ API.placeOrder body
  where
    body = PlaceOrderBody prid sd sz price po ot stp tif


-- | https://docs.pro.coinbase.com/?javascript#cancel-an-order
cancelOrder :: OrderId -> CBAuthT ClientM ()
cancelOrder oid = void . authRequest methodDelete (mkRequestPath "/orders") "" $ API.cancelOrder oid
  where
    mkRequestPath :: RequestPath -> RequestPath
    mkRequestPath rp = rp ++ "/" ++ unpack (unOrderId oid)


-- | https://docs.pro.coinbase.com/?javascript#cancel-all
cancelAll :: Maybe ProductId -> CBAuthT ClientM [OrderId]
cancelAll prid = authRequest methodDelete (mkRequestPath "/orders") "" (API.cancelAll prid)
  where
    mkRequestPath :: RequestPath -> RequestPath
    mkRequestPath rp = rp ++ (C8.unpack . renderQuery True . simpleQueryToQuery $ mkProductQuery prid)


-- | https://docs.pro.coinbase.com/?javascript#fills
fills :: Maybe ProductId -> Maybe OrderId -> CBAuthT ClientM [Fill]
fills prid oid = authRequest methodGet mkRequestPath "" (API.fills prid oid)
  where
    brp = "/fills"

    mkRequestPath :: RequestPath
    mkRequestPath = brp ++ (C8.unpack . renderQuery True . simpleQueryToQuery $ mkSimpleQuery prid oid)

    mkSimpleQuery :: Maybe ProductId -> Maybe OrderId -> SimpleQuery
    mkSimpleQuery p o = mkProductQuery p <> mkOrderIdQuery o


-- | https://docs.pro.coinbase.com/?javascript#trailing-volume
trailingVolume :: CBAuthT ClientM [TrailingVolume]
trailingVolume = authRequest methodGet mkRequestPath "" API.trailingVolume
  where
    mkRequestPath :: RequestPath
    mkRequestPath = "/users/self/trailing-volume"


mkSimpleQueryItem :: String -> Text -> SimpleQueryItem
mkSimpleQueryItem s t = (C8.pack s, C8.pack $ unpack t)


mkProductQuery :: Maybe ProductId -> [SimpleQueryItem]
mkProductQuery = maybe [] (return . mkSimpleQueryItem "product_id" . unProductId)


mkOrderIdQuery :: Maybe OrderId -> SimpleQuery
mkOrderIdQuery = maybe [] (return . mkSimpleQueryItem "order_id" . unOrderId)
