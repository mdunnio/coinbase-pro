module CoinbasePro.Environment
    ( Environment (..)
    , WSConnection (..)

    , apiEndpoint
    , wsEndpoint
    ) where


import           Network.Socket (HostName)


data Environment = Production | Sandbox


apiEndpoint :: Environment -> String
apiEndpoint Production = productionAPIEndpoint
apiEndpoint Sandbox    = sandboxAPIEndpoint


productionAPIEndpoint :: String
productionAPIEndpoint = "api.pro.coinbase.com"


sandboxAPIEndpoint :: String
sandboxAPIEndpoint = "api-public.sandbox.pro.coinbase.com"


data WSConnection = WSConnection
    { host :: HostName
    , port :: Int
    } deriving (Eq, Show)


wsEndpoint :: Environment -> WSConnection
wsEndpoint Production = productionWSEndpoint
wsEndpoint Sandbox    = sandboxWSEndpoint


productionWSEndpoint :: WSConnection
productionWSEndpoint = WSConnection "ws-feed.pro.coinbase.com" 443


sandboxWSEndpoint :: WSConnection
sandboxWSEndpoint = WSConnection "ws-feed-public.sandbox.pro.coinbase.com" 443
