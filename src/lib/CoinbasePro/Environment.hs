{-# LANGUAGE OverloadedStrings #-}

module CoinbasePro.Environment
    ( Environment (..)
    , WSConnection (..)

    , apiEndpoint
    , wsEndpoint
    ) where


import           Data.Text      (Text)
import           Network.Socket (HostName, PortNumber)


data Environment = Production | Sandbox


apiEndpoint :: Environment -> Text
apiEndpoint Production = productionAPIEndpoint
apiEndpoint Sandbox    = sandboxAPIEndpoint


productionAPIEndpoint :: Text
productionAPIEndpoint = "api.pro.coinbase.com"


sandboxAPIEndpoint :: Text
sandboxAPIEndpoint = "api-public.sandbox.pro.coinbase.com"


data WSConnection = WSConnection
    { host :: HostName
    , port :: PortNumber
    } deriving (Eq, Show)


wsEndpoint :: Environment -> WSConnection
wsEndpoint Production = productionWSEndpoint
wsEndpoint Sandbox    = sandboxWSEndpoint


productionWSEndpoint :: WSConnection
productionWSEndpoint = WSConnection "ws-feed.pro.coinbase.com" 443


sandboxWSEndpoint :: WSConnection
sandboxWSEndpoint = WSConnection "ws-feed-public.sandbox.pro.coinbase.com" 443
