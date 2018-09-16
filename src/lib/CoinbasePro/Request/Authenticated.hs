{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}

module CoinbasePro.Request.Authenticated
    ( CBAuth (..)
    , CBAuthT (..)
    , runCBAuthT
    , authRequest
    ) where


import           Control.Monad.IO.Class     (MonadIO, liftIO)
import           Control.Monad.Trans.Class  (MonadTrans)
import           Control.Monad.Trans.Reader (ReaderT (..), asks)
import           Crypto.Hash.Algorithms     (SHA256)
import qualified Crypto.MAC.HMAC            as HMAC
import           Data.Aeson                 (FromJSON)
import qualified Data.Aeson                 as A
import           Data.Binary.Builder        (fromByteString)
import           Data.ByteArray.Encoding    (Base (Base64), convertToBase)
import           Data.ByteString            (ByteString)
import           Data.ByteString.Base64     (decode)
import           Data.Monoid                ((<>))
import           Data.Text                  (Text, pack)
import           Data.Text.Encoding         (encodeUtf8)
import           Data.Time.Clock            (getCurrentTime)
import           Data.Time.Format           (defaultTimeLocale, formatTime)
import           Network.Http.Client        (Hostname, Method (..), Request,
                                             baselineContextSSL, buildRequest1,
                                             concatHandler, http,
                                             openConnectionSSL, receiveResponse,
                                             sendRequest, setAccept,
                                             setContentType, setHeader,
                                             setHostname)
import qualified System.IO.Streams          as Streams

import           CoinbasePro.Headers        (CBAccessKey (..),
                                             CBAccessPassphrase (..),
                                             CBAccessSign (..),
                                             CBAccessTimeStamp (..))
import           CoinbasePro.Request        (Connection (..))


data CBAuth = CBAuth
    { connection         :: Connection
    , cbAccessKey        :: CBAccessKey
    , cbAccessPassphrase :: CBAccessPassphrase
    }

newtype CBAuthT m a = CBAuthT { unCBAuth :: ReaderT CBAuth m a }
    deriving ( Functor, Applicative, Monad, MonadTrans, MonadIO )


runCBAuthT :: CBAuth -> CBAuthT m a -> m a
runCBAuthT ga = flip runReaderT ga . unCBAuth


authRequest :: (MonadIO m, FromJSON a) => Text -> Text -> CBAuthT m (Maybe a)
authRequest endpoint body = do
    ctx   <- liftIO baselineContextSSL
    host' <- CBAuthT $ asks (host . connection)
    port' <- CBAuthT $ asks (port . connection)
    cbak  <- CBAuthT $ asks cbAccessKey
    cbp   <- CBAuthT $ asks cbAccessPassphrase
    c     <- liftIO $ openConnectionSSL ctx host' port'
    t     <- liftIO $ formatTime defaultTimeLocale "%s" <$> getCurrentTime
    decodedApiKey <- liftIO $ decodeApiKey $ unCBAccessKey cbak
    let ts         = CBAccessTimeStamp t
        msg        = mkMessage ts GET endpoint body
        accessSign = CBAccessSign . convertToBase Base64 $ HMAC.hmacGetDigest (HMAC.hmac decodedApiKey msg :: HMAC.HMAC SHA256)
        aReq       = mkAuthRequest GET (encodeUtf8 endpoint) host' cbak cbp ts accessSign
    liftIO $ sendRequest c aReq $ Streams.write (Just . fromByteString $ encodeUtf8 body)
    res <- liftIO $ receiveResponse c concatHandler
    liftIO $ print res
    either fail return $ A.eitherDecodeStrict res


mkAuthRequest :: Method
              -> ByteString
              -> Hostname
              -> CBAccessKey
              -> CBAccessPassphrase
              -> CBAccessTimeStamp
              -> CBAccessSign
              -> Request
mkAuthRequest m endpoint host' key pass ts sign = buildRequest1 $ do
    http m endpoint
    setContentType contentType
    setHostname host' 80
    setAccept contentType
    setHeader "User-Agent" "coinbase-pro/0.1"
    setHeader "CB-ACCESS-KEY" $ unCBAccessKey key
    setHeader "CB-ACCESS-PASSPHRASE" $ unCBAccessPassphrase pass
    setHeader "CB-ACCESS-TIMESTAMP" . encodeUtf8 . pack $ unCBAccessTimestamp ts
    setHeader "CB-ACCESS-SIGN" $ unCBAccessSign sign
  where
    contentType = "application/json"


mkMessage :: CBAccessTimeStamp -> Method -> Text -> Text -> ByteString
mkMessage t m reqPath body = t' <> m' <> reqPath' <> body'
  where
    t'       = encodeUtf8 . pack . show $ unCBAccessTimestamp t
    m'       = encodeUtf8 . pack . show $ m
    reqPath' = encodeUtf8 reqPath
    body'    = encodeUtf8 body


decodeApiKey :: ByteString -> IO ByteString
decodeApiKey = either fail return . decode
