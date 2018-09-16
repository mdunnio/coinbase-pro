module CoinbasePro.Types
    ( OrderId
    , Price
    , ProductId
    , Sequence
    , Side
    , Size
    ) where

import           Data.Text (Text)

type OrderId   = Text
type Price     = Double
type ProductId = Text
type Sequence  = Int
type Side      = Text
type Size      = Double
