{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}

module CoinbasePro.Request
    ( CBAuthT (..)
    , runCbAuthT

    , CoinbaseProCredentials (..)
    , CBSecretKey (..)
    , RequestPath
    , Body

    , AuthGet
    , AuthPost
    , AuthDelete

    , run
    , authRequest
    , apiEndpoint
    ) where

import           Control.Exception          (throw)
import           Control.Monad.IO.Class     (MonadIO, liftIO)
import           Control.Monad.Trans.Class  (MonadTrans, lift)
import           Control.Monad.Trans.Reader (ReaderT, asks, runReaderT)
import           Crypto.Hash.Algorithms     (SHA256)
import qualified Crypto.MAC.HMAC            as HMAC
import           Data.ByteArray.Encoding    (Base (Base64), convertFromBase,
                                             convertToBase)
import           Data.ByteString            (ByteString)
import qualified Data.ByteString.Char8      as C8
import           Data.Time.Clock            (getCurrentTime)
import           Data.Time.Format           (defaultTimeLocale, formatTime)
import           GHC.TypeLits               (Symbol)
import           Network.HTTP.Client        (newManager)
import           Network.HTTP.Client.TLS    (tlsManagerSettings)
import           Network.HTTP.Types         (Method)
import           Servant.API                ((:>), AuthProtect, Delete, Get,
                                             JSON, Post)
import           Servant.Client
import           Servant.Client.Core        (AuthClientData,
                                             AuthenticatedRequest, addHeader,
                                             mkAuthenticatedRequest)
import qualified Servant.Client.Core        as SCC

import           CoinbasePro.Headers        (CBAccessKey (..),
                                             CBAccessPassphrase (..),
                                             CBAccessSign (..),
                                             CBAccessTimeStamp (..), userAgent)


newtype CBSecretKey = CBSecretKey String
    deriving (Eq)


data CoinbaseProCredentials = CoinbaseProCredentials
    { cbAccessKey        :: CBAccessKey
    , cbSecretKey        :: CBSecretKey
    , cbAccessPassphrase :: CBAccessPassphrase
    } deriving (Eq)


newtype CBAuthT m a = CBAuthT { unCbAuth :: ReaderT CoinbaseProCredentials m a }
    deriving (Functor, Applicative, Monad, MonadIO, MonadTrans)


runCbAuthT :: CoinbaseProCredentials -> CBAuthT ClientM a -> IO a
runCbAuthT cpc = run . flip runReaderT cpc . unCbAuth


type RequestPath = String
type Body        = String


apiEndpoint :: String
apiEndpoint = "api.pro.coinbase.com"


run :: ClientM a -> IO a
run f = do
    mgr <- newManager tlsManagerSettings
    result <- runClientM f (mkClientEnv mgr (BaseUrl Https apiEndpoint 443 mempty))
    either throw return result


type instance AuthClientData (AuthProtect "CBAuth") = (CBAccessKey, CBAccessSign, CBAccessTimeStamp, CBAccessPassphrase)


type CBAuthAPI (auth :: Symbol) method a = AuthProtect auth :> method '[JSON] a


type AuthGet a    = CBAuthAPI "CBAuth" Get a
type AuthPost a   = CBAuthAPI "CBAuth" Post a
type AuthDelete a = CBAuthAPI "CBAuth" Delete a


addAuthHeaders :: (CBAccessKey, CBAccessSign, CBAccessTimeStamp, CBAccessPassphrase) -> SCC.Request -> SCC.Request
addAuthHeaders (key, sig, timestamp, pass) req =
      addHeader "CB-ACCESS-KEY" key
    $ addHeader "CB-ACCESS-SIGN" sig
    $ addHeader "CB-ACCESS-TIMESTAMP" timestamp
    $ addHeader "CB-ACCESS-PASSPHRASE" pass
    $ addHeader "User-Agent" userAgent req


authRequest :: Method -> RequestPath -> Body
            -> (AuthenticatedRequest (AuthProtect "CBAuth") -> ClientM b)
            -> CBAuthT ClientM b
authRequest method requestPath body f = do
    ak <- CBAuthT $ asks cbAccessKey
    sk <- CBAuthT $ asks cbSecretKey
    pp <- CBAuthT $ asks cbAccessPassphrase

    ts <- liftIO mkCBAccessTimeStamp
    let cbs = mkCBAccessSign sk ts method requestPath body
    lift . f $ mkAuthenticatedRequest (ak, cbs, ts, pp) addAuthHeaders


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
