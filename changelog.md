# Version 0.9.3.2

- Upgraded to lts-20.4 (ghc 9.2.5)

# Version 0.9.3.1

- Added test data to `extra-source-files` so that `cabal test` can be run straight from the hackage
  package

# Version 0.9.3.0

- `Order` now has `Maybe Bool` instead of `Bool` for `postOnly`. Required to maintain spec compliance.
  From https://docs.pro.coinbase.com/#upcoming-changes:

```
08/09/21

    Orders with a "pending" status returned by the REST API endpoints GET /orders, GET /orders/<id>,
	and GET /orders/client:<client_oid> will have a reduced set of fields. See the List Orders documentation
	for more details. Orders with non-pending statuses will be unaffected by this change. The change will
	take effect in Sandbox starting after August 12th, 2021 and in Production starting after August 19th, 2021.
```

- Added tests to test order parsing from json file

# Version 0.9.2.2

- Added `CoinbasePro.Unauthenticated.singleProduct`

# Version 0.9.2.1

- Use `Runner` instead of `ClientM a -> IO a` where applicable
- Added more documentation around `Runner`
- Added `runDefCbAuthT` as a default for calling `runCbAuthT` without passing in a `Runner`
- Updated `src/example/request/Main.hs` to reflect `runDefCbAuthT` and a minor refactor

# Version 0.9.2.0

Feature complete! (except FIX)

## Additional Features

- Added support for the following endpoints:
  - stablecoin conversions
  - oracle

# Version 0.9.1.0

- Minor refactors + linting.

## Additional Features

- Added support for the following endpoints:
  - `Profiles`
  - `Reports`

# Version 0.9.0.0

- Upgraded to lts-17.4 (ghc 8.10.4)
- Added hie.yaml for use with haskell-language-server (hls)
- Removed `taker_fee_rate` from `Activate` websocket message
- Added `max_withdrawal_amount` to `CurrencyDetails`
- Changed `example/test-request` to `Sandbox` instead of `Production`
- Use `Network.HTTP.Types.encodePathSegments` when making `RequestPath` in authenticated requests
- Added `min_market_funds` and `max_market_funds` to `Product`
- Made several explicit `Show` instances
- Added support for the following endpoints:
  - account history
  - account holds
  - exchange limits
  - deposits
  - withdrawals
  - list payment methods
  - listing coinbase accounts
  - list tranfers endpoint

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
