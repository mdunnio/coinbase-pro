{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE TypeOperators     #-}

module CoinbasePro.Request
    ( RequestPath
    , Body

    , CBGet
    , CBRequest

    -- * Production
    , run
    , run_
    , runWithManager

    -- * Sandbox
    , runSandbox
    , runSandbox_
    , runSandboxWithManager
    ) where

import           Control.Exception       (throw)
import           Control.Monad           (void)
import           Network.HTTP.Client     (Manager, newManager)
import           Network.HTTP.Client.TLS (tlsManagerSettings)
import           Servant.API             ((:>), Get, JSON)
import           Servant.Client

import           CoinbasePro.Headers     (UserAgent, UserAgentHeader)


type CBGet a = UserAgentHeader :> Get '[JSON] a


type CBRequest a = UserAgent -> ClientM a


type RequestPath = String
type Body        = String


------------------------------------------------------------------------------
-- | Runs a coinbase pro HTTPS request and returns the result `a`
--
-- > run products >>= print
--
run :: ClientM a -> IO a
run f = flip runWithManager f =<< newManager tlsManagerSettings


------------------------------------------------------------------------------
-- | Same as 'run', except uses `()` instead of a type `a`
run_ :: ClientM a -> IO ()
run_ = void . run


------------------------------------------------------------------------------
-- | Allows the user to use their own 'Network.HTTP.Client.Types.ManagerSettings`
-- with 'run'
--
-- @
-- do $
-- mgr  <- newManager tlsManagerSettings
-- prds <- runWithManager mgr products
-- print prds
-- @
--
runWithManager :: Manager -> ClientM a -> IO a
runWithManager mgr f = either throw return =<<
    runClientM f (mkClientEnv mgr (BaseUrl Https production 443 mempty))
  where
    production = "api.pro.coinbase.com"

------------------------------------------------------------------------------
-- | Runs a coinbase pro HTTPS request in sandbox and returns the result 'a'
--
-- > run products >>= print
--
runSandbox :: ClientM a -> IO a
runSandbox f = flip runSandboxWithManager f =<< newManager tlsManagerSettings


------------------------------------------------------------------------------
-- | Same as 'runSandbox', except uses '()' instead of a type 'a'
runSandbox_ :: ClientM a -> IO ()
runSandbox_ = void . runSandbox


------------------------------------------------------------------------------
-- | Allows the user to use their own 'Network.HTTP.Client.Types.ManagerSettings`
--
-- @
-- do $
-- mgr  <- newManager tlsManagerSettings
-- prds <- runSandboxWithManager mgr products
-- print prds
-- @
--
runSandboxWithManager :: Manager -> ClientM a -> IO a
runSandboxWithManager mgr f = either throw return =<<
    runClientM f (mkClientEnv mgr (BaseUrl Https sandbox 443 mempty))
  where
    sandbox = "api-public.sandbox.pro.coinbase.com"
