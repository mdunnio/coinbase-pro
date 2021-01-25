# Version 0.9.0.0

- Upgraded to nightly-2020-01-17 (ghc 8.10.3) in preparation for a new 8.10 LTS.
- Added hie.yaml for use with haskell-language-server (hls)
- Removed `taker_fee_rate` from `Activate` websocket message
- Support account history endpoint
- Added `max_withdrawal_amount` to `CurrencyDetails`
- Support account holds endpoint
- Changed `example/test-request` to `Sandbox` instead of `Production`
- Use `Network.HTTP.Types.encodePathSegments` when making `RequestPath` in authenticated requests
- Support exchange limits endpoint
- Added `min_market_funds` and `max_market_funds` to `Product`
- Made several explicit `Show` instances
- Support list deposits endpoint
- Support get deposit request
- Support make deposit request
- Support list payment methods

# Version 0.8.2.0

- Changed `port` in `WSConnection` back to `PortNumber` instead of `Int`

# Version 0.8.1.0

- Upgraded to lts-16.7 (ghc 8.8.3)
- Changed `port` in `WSConnection` to `Int` instead of `PortNumber`

# Version 0.8.0.0

## Breaking API changes:

- `Environment` concept has been introduced. This allows the user to specify an endpoint (production, sandbox)
  in which to query or stream.

    - `run` (and associated functions) now take an `Environment` as the first parameter. `runSandbox` and associated
      functions have been removed.
    - `runWithManager` now has different parameter ordering. Signature is `runWithManager :: Manager -> Environment -> ClientM a -> IO a`
    - `runCbAuthT` now takes a `Runner a`, where `Runner a = ClientM a -> IO a`; allows users to specify an environment in which to run

- `placeOrder` now has `Maybe ClientOrderId` as the first parameter that allows users to assign `client_oid` to
  submitted orders. The `client_oid` field is visible on the `Full` channel.

- `CoinbasePro.WebSocketFeed.subscribeToFeed` has two additional parameters:

    - `Environment`
    - `Maybe CoinbaseProCredentials`: allows users to authenticate on subscription. See `User`

- Moved `UserId` and `ProfileId` to `CoinbasePro.Types` from `CoinbasePro.WebSocketFeed.Channel.Full.Activate`

## Additional Features:

- Added `User` channel functionality that allows users to see `user_id` and `profile_id` on messages that pertain to the subscriber.
  See [documentation](https://docs.pro.coinbase.com/#the-user-channel) for more details

    - `user_id` and `profile_id` fields are now parsed from `Full` channel messages if available.

- `CoinbasePro.Types.ClientOrderId` has been introduced to specify `client_oid` in `placeOrder` API request.
  - `client_oid` is now parsed from `CoinbasePro.WebSocketFeed.Channel.Full.Received.

- `CoinbasePro.Authenticated.getOrder` allows users to query status of order

- `CoinbasePro.Authenticated.getClientOrder` allows users to query status of order using `client_oid`


## Misc

- minor cleanups of unused imports

# Version 0.7.2.0
 - Upgraded to stack lts-14.17

# Version 0.7.1.0
 - Fixed broken examples

# Version 0.7.0.0
 - Unauthenticated requests are now all in the `ClientM` monad. `CoinbasePro.Request.run` is now required
   to operate in the IO monad.

   Example: `run (trades (ProductId "BTC-USD")) >>= print`

- Added `run_`, `runSandbox`, `runSandboxWithManager` in `CoinbasePro.Request`
- Added `currencies`, `fees`, and `trailingVolume` queries
