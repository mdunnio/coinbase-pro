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
    , runWithManager
    , runSandbox
    , runSandboxWithManager
    ) where

import           Control.Exception       (throw)
import           Network.HTTP.Client     (Manager, newManager)
import           Network.HTTP.Client.TLS (tlsManagerSettings)
import           Servant.API             ((:>), Get, JSON)
import           Servant.Client

import           CoinbasePro.Headers     (UserAgent, UserAgentHeader)


type CBGet a = UserAgentHeader :> Get '[JSON] a


type CBRequest a = UserAgent -> ClientM a


type RequestPath = String
type Body        = String


run :: ClientM a -> IO a
run f = flip runWithManager f =<< newManager tlsManagerSettings


runWithManager :: Manager -> ClientM a -> IO a
runWithManager mgr f = either throw return =<<
    runClientM f (mkClientEnv mgr (BaseUrl Https production 443 mempty))
  where
    production = "api.pro.coinbase.com"


runSandbox :: ClientM a -> IO a
runSandbox f = flip runSandboxWithManager f =<< newManager tlsManagerSettings


runSandboxWithManager :: Manager -> ClientM a -> IO a
runSandboxWithManager mgr f = either throw return =<<
    runClientM f (mkClientEnv mgr (BaseUrl Https sandbox 443 mempty))
  where
    sandbox = "api-public.sandbox.pro.coinbase.com"
