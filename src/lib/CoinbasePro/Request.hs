{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE TypeOperators     #-}

module CoinbasePro.Request
    ( RequestPath
    , Body

    , CBGet
    , CBRequest

    , run
    , apiEndpoint
    ) where

import           Control.Exception       (throw)
import           Network.HTTP.Client     (newManager)
import           Network.HTTP.Client.TLS (tlsManagerSettings)
import           Servant.API             ((:>), Get, JSON)
import           Servant.Client

import           CoinbasePro.Headers     (UserAgent, UserAgentHeader)


type CBGet a = UserAgentHeader :> Get '[JSON] a


type CBRequest a = UserAgent -> ClientM a


type RequestPath = String
type Body        = String


apiEndpoint :: String
apiEndpoint = "api.pro.coinbase.com"


run :: ClientM a -> IO a
run f = do
    mgr <- newManager tlsManagerSettings
    result <- runClientM f (mkClientEnv mgr (BaseUrl Https apiEndpoint 443 mempty))
    either throw return result
