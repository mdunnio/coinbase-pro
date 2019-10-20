# Version 0.7.1.0
 - Fixed broken examples

# Version 0.7.0.0
 - Unauthenticated requests are now all in the `ClientM` monad. `CoinbasePro.Request.run` is now required
   to operate in the IO monad.

   Example: `run (trades (ProductId "BTC-USD")) >>= print`

- Added `run_`, `runSandbox`, `runSandboxWithManager` in `CoinbasePro.Request`
- Added `currencies`, `fees`, and `trailingVolume` queries
