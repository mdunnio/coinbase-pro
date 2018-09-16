{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}

module CoinbasePro.Request
    ( Connection(..)
    , CBT (..)
    , apiEndpoint
    , runCBT
    , request
    ) where

import           Control.Monad.IO.Class     (MonadIO, liftIO)
import           Control.Monad.Trans.Class  (MonadTrans)
import           Control.Monad.Trans.Reader (ReaderT (..), asks)
import           Data.Aeson                 (FromJSON, eitherDecodeStrict)
import           Data.ByteString            (ByteString)
import           Data.Text                  (Text)
import           Data.Text.Encoding         (encodeUtf8)
import           Network.Http.Client        (Hostname, Method (..), Port,
                                             Request, baselineContextSSL,
                                             buildRequest1, concatHandler,
                                             emptyBody, http, openConnectionSSL,
                                             receiveResponse, sendRequest,
                                             setAccept, setContentType,
                                             setHeader, setHostname)


newtype CBT m a = CBT { unCBRequest :: ReaderT Connection m a }
    deriving ( Functor, Applicative, Monad, MonadTrans, MonadIO )


runCBT :: Connection -> CBT m a -> m a
runCBT gc = flip runReaderT gc . unCBRequest


data Connection = Connection
    { host :: Hostname
    , port :: Port
    } deriving (Eq, Show)


apiEndpoint :: Connection
apiEndpoint = Connection h p
  where
    h = "api.pro.coinbase.com"
    p = 443


request :: (MonadIO m, FromJSON a) => Text -> CBT m a
request endpoint = do
    ctx   <- liftIO baselineContextSSL
    host' <- CBT $ asks host
    port' <- CBT $ asks port
    c     <- liftIO $ openConnectionSSL ctx host' port'
    let req = mkRequest GET (encodeUtf8 endpoint) host'
    liftIO $ sendRequest c req emptyBody
    res <- liftIO $ receiveResponse c concatHandler
    either fail return $ eitherDecodeStrict res


mkRequest :: Method -> ByteString -> Hostname -> Request
mkRequest m endpoint host' = buildRequest1 $ do
    http m endpoint
    setContentType contentType
    setHostname host' pt
    setAccept contentType
    setHeader "User-Agent" "coinbase-pro/0.1"
  where
    pt = 80
    contentType = "application/json"
