{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeFamilies  #-}
{-# LANGUAGE TypeOperators #-}

module CoinbasePro.Request
    ( RequestPath
    , Body

    , CBGet
    , CBRequest

    , run
    , run_
    , runWithManager

    , Runner
    ) where

import           Control.Exception       (throw)
import           Control.Monad           (void)
import           Network.HTTP.Client     (Manager, newManager)
import           Network.HTTP.Client.TLS (tlsManagerSettings)
import           Servant.API             ((:>), Get, JSON)
import           Servant.Client

import           CoinbasePro.Environment (Environment, apiEndpoint)
import           CoinbasePro.Headers     (UserAgent, UserAgentHeader)


type CBGet a = UserAgentHeader :> Get '[JSON] a


type CBRequest a = UserAgent -> ClientM a


type RequestPath = String
type Body        = String


type Runner a = ClientM a -> IO a


------------------------------------------------------------------------------
-- | Runs a coinbase pro HTTPS request and returns the result `a`
--
-- > run Production products >>= print
--
run :: Environment -> ClientM a -> IO a
run env f = do
    mgr <- newManager tlsManagerSettings
    runWithManager mgr env f


------------------------------------------------------------------------------
-- | Same as 'run', except uses `()` instead of a type `a`
run_ :: Environment -> ClientM a -> IO ()
run_ = (void .) . run


------------------------------------------------------------------------------
-- | Allows the user to use their own 'Network.HTTP.Client.Types.ManagerSettings`
-- with 'run'
--
-- @
-- do $
-- mgr  <- newManager tlsManagerSettings
-- prds <- runWithManager mgr Production products
-- print prds
-- @
--
runWithManager :: Manager -> Environment -> ClientM a -> IO a
runWithManager mgr env f = either throw return =<<
    runClientM f (mkClientEnv mgr (BaseUrl Https api 443 mempty))
  where
    api = apiEndpoint env
