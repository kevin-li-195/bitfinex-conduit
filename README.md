# bitfinex-conduit
Haskell wrapper for Bitfinex API


Functionality
========

- Bitfinex public endpoints for public info
- Bitfinex private endpoints to private info + trading (IN PROGRESS)

TODO
========

- Clean up JSON parsing to eliminate unnecessary types.
- Integrate types with Jafar types.
- Credential handling for private endpoints.

Example
=========

Gets list of symbols and asks info for the first symbol:

```haskell
import Bitfinex

right = fmap (either (error "It's left") id)

main = do
  sym <- fmap head $ right $ getSymbols
  ticker <- getTicker sym
  stat <- fmap head $ right $ getStats sym

  print sym
  print ticker
  print stat

```
