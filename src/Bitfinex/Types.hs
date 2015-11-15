{-# LANGUAGE OverloadedStrings #-}

module Bitfinex.Types where

import Data.Aeson
import Data.Text (unpack)
import Control.Monad
import Control.Applicative

data Symbol = Symbol
    { getPair :: String
    , getPrecision :: Int
    , getInitialMargin :: Price
    , getMinimumMargin :: Price
    , getMaxOrderSize :: Price
    , getMinOrderSize :: Price
    , getExpiration :: String
    }
    deriving Show

newtype Price = Price { unPrice :: Double }
    deriving Show

-- TODO: Fix dangerous reads
instance FromJSON Price where
    parseJSON (String b) = Price <$> pure ((read . unpack) b)

instance FromJSON Symbol where
    parseJSON (Object o) = Symbol <$>
                           o .: "pair" <*>
                           o .: "price_precision" <*>
                           o .: "initial_margin" <*>
                           o .: "minimum_margin" <*>
                           o .: "maximum_order_size" <*>
                           o .: "minimum_order_size" <*>
                           o .: "expiration"
    parseJSON _          = mzero
