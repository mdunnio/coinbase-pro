{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}

module CoinbasePro.Authenticated.Request
    ( CBAuthT (..)
    , runCbAuthT
    , runDefCbAuthT

    , CoinbaseProCredentials (..)
    , CBSecretKey (..)

    , AuthGet
    , AuthPost
    , AuthDelete

    , authRequest
    , mkCBAccessSign
    , mkCBAccessTimeStamp
    ) where

import           Control.Monad.IO.Class            (MonadIO, liftIO)
import           Control.Monad.Trans.Class         (MonadTrans, lift)
import           Control.Monad.Trans.Reader        (ReaderT, asks, runReaderT)
import           Crypto.Hash.Algorithms            (SHA256)
import qualified Crypto.MAC.HMAC                   as HMAC
import           Data.ByteArray.Encoding           (Base (Base64),
                                                    convertFromBase,
                                                    convertToBase)
import           Data.ByteString                   (ByteString)
import qualified Data.ByteString.Char8             as C8
import           Data.Text                         (pack)
import           Data.Text.Encoding                (encodeUtf8)
import           Data.Time.Clock                   (getCurrentTime)
import           Data.Time.Format                  (defaultTimeLocale,
                                                    formatTime)
import           GHC.TypeLits                      (Symbol)
import           Network.HTTP.Types                (Method)
import           Servant.API                       (AuthProtect, Delete, Get,
                                                    JSON, Post, (:>))
import           Servant.Client                    (ClientM)
import           Servant.Client.Core               (AuthClientData,
                                                    AuthenticatedRequest,
                                                    addHeader,
                                                    mkAuthenticatedRequest)
import qualified Servant.Client.Core               as SCC

import           CoinbasePro.Authenticated.Headers (CBAccessKey (..),
                                                    CBAccessPassphrase (..),
                                                    CBAccessSign (..),
                                                    CBAccessTimeStamp (..))
import           CoinbasePro.Environment           (Environment)
import           CoinbasePro.Headers               (userAgent)
import           CoinbasePro.Request               (Body, RequestPath, Runner,
                                                    run)


newtype CBSecretKey = CBSecretKey String
    deriving (Eq)


data CoinbaseProCredentials = CoinbaseProCredentials
    { cbAccessKey        :: CBAccessKey
    , cbSecretKey        :: CBSecretKey
    , cbAccessPassphrase :: CBAccessPassphrase
    } deriving (Eq)


newtype CBAuthT m a = CBAuthT { unCbAuth :: ReaderT CoinbaseProCredentials m a }
    deriving (Functor, Applicative, Monad, MonadIO, MonadTrans)


-- | Sequences `ClientM` actions using the same auth credentials
--
-- This allows for custom `Runner`s to be used.
runCbAuthT :: Runner a -> CoinbaseProCredentials -> CBAuthT ClientM a -> IO a
runCbAuthT runEnv cpc = runEnv . flip runReaderT cpc . unCbAuth


-- | Sequences `ClientM` actions using the same auth credentials
--
-- Should be used over `runCbAuthT` unless a bespoke `Runner` needs to be used.
runDefCbAuthT :: Environment -> CoinbaseProCredentials -> CBAuthT ClientM a -> IO a
runDefCbAuthT env = runCbAuthT (run env)


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
mkCBAccessTimeStamp = CBAccessTimeStamp . pack . formatTime defaultTimeLocale "%s%Q" <$> getCurrentTime


mkCBAccessSign :: CBSecretKey -> CBAccessTimeStamp -> Method -> RequestPath -> Body -> CBAccessSign
mkCBAccessSign sk ts method requestPath body = CBAccessSign $ convertToBase Base64 hmac
  where
    dak  = decodeApiKey sk
    msg  = mkMsg ts method requestPath body
    hmac = HMAC.hmac dak msg :: HMAC.HMAC SHA256

    mkMsg :: CBAccessTimeStamp -> Method -> RequestPath -> Body -> ByteString
    mkMsg (CBAccessTimeStamp s) m rp b = encodeUtf8 s <> m <> rp <> b


decodeApiKey :: CBSecretKey -> ByteString
decodeApiKey (CBSecretKey s) = either error id . convertFromBase Base64 $ C8.pack s
