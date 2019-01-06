{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}

module CoinbasePro.Request
    ( CBAuthT (..)
    , runCbAuthT

    , CoinbaseProCredentials (..)
    , CBSecretKey (..)
    , RequestPath
    , Body

    , request
    , authRequest
    , apiEndpoint
    ) where

import           Control.Exception          (throw)
import           Control.Monad.IO.Class     (MonadIO, liftIO)
import           Control.Monad.Trans.Class  (MonadTrans)
import           Control.Monad.Trans.Reader (ReaderT, asks, runReaderT)
import           Crypto.Hash.Algorithms     (SHA256)
import qualified Crypto.MAC.HMAC            as HMAC
import           Data.ByteArray.Encoding    (Base (Base64), convertFromBase,
                                             convertToBase)
import           Data.ByteString            (ByteString)
import qualified Data.ByteString.Char8      as C8
import           Data.Time.Clock            (getCurrentTime)
import           Data.Time.Format           (defaultTimeLocale, formatTime)
import           Network.HTTP.Client        (newManager)
import           Network.HTTP.Client.TLS    (tlsManagerSettings)
import           Network.HTTP.Types         (Method)
import           Servant.Client

import           CoinbasePro.Headers        (CBAccessKey (..),
                                             CBAccessPassphrase (..),
                                             CBAccessSign (..),
                                             CBAccessTimeStamp (..))
import           CoinbasePro.Types          (UserAgent, userAgent)


newtype CBSecretKey = CBSecretKey String
    deriving (Eq)


data CoinbaseProCredentials = CoinbaseProCredentials
    { cbAccessKey        :: CBAccessKey
    , cbSecretKey        :: CBSecretKey
    , cbAccessPassphrase :: CBAccessPassphrase
    } deriving (Eq)


newtype CBAuthT m a = CBAuthT { unCbAuth :: ReaderT CoinbaseProCredentials m a }
    deriving (Functor, Applicative, Monad, MonadIO, MonadTrans)


runCbAuthT :: MonadIO m => CoinbaseProCredentials -> CBAuthT m a -> m a
runCbAuthT cpc = flip runReaderT cpc . unCbAuth


type APIRequest a = UserAgent -> ClientM a
type AuthAPIRequest a = CBAccessKey -> CBAccessSign -> CBAccessTimeStamp -> CBAccessPassphrase -> UserAgent -> ClientM a


type RequestPath = String
type Body        = String


apiEndpoint :: String
apiEndpoint = "api.pro.coinbase.com"


request :: APIRequest a -> IO a
request f = do
    mgr <- newManager tlsManagerSettings
    result <- runClientM (f userAgent) (mkClientEnv mgr (BaseUrl Https apiEndpoint 443 ""))
    either throw return result


authRequest :: MonadIO m => Method -> RequestPath -> Body -> AuthAPIRequest a -> CBAuthT m a
authRequest method requestPath body f = do
    ak <- CBAuthT $ asks cbAccessKey
    sk <- CBAuthT $ asks cbSecretKey
    pp <- CBAuthT $ asks cbAccessPassphrase

    ts <- liftIO mkCBAccessTimeStamp
    let cbs = mkCBAccessSign sk ts method requestPath body
    liftIO $ request (f ak cbs ts pp)


mkCBAccessTimeStamp :: IO CBAccessTimeStamp
mkCBAccessTimeStamp = CBAccessTimeStamp . formatTime defaultTimeLocale "%s%Q" <$> getCurrentTime


mkCBAccessSign :: CBSecretKey -> CBAccessTimeStamp -> Method -> RequestPath -> Body -> CBAccessSign
mkCBAccessSign sk ts method requestPath body = CBAccessSign $ convertToBase Base64 hmac
  where
    dak  = decodeApiKey sk
    msg  = mkMsg ts method requestPath body
    hmac = HMAC.hmac dak msg :: HMAC.HMAC SHA256

    mkMsg :: CBAccessTimeStamp -> Method -> RequestPath -> Body -> ByteString
    mkMsg (CBAccessTimeStamp s) m rp b = C8.pack $ s ++ C8.unpack m ++ rp ++ b


decodeApiKey :: CBSecretKey -> ByteString
decodeApiKey (CBSecretKey s) = either error id . convertFromBase Base64 $ C8.pack s
