{-# LANGUAGE OverloadedStrings #-}

module Bitfinex.Core where

import Network.HTTP.Conduit
import Data.Aeson
import qualified Data.ByteString.Lazy as L
import Data.Default (def)
import Bitfinex.Types
import Control.Monad
import Control.Applicative

-- defaults = def { host = "https://api.bitfinex.com/v1"
--                , port = 443
--                , secure = True
--                }

makeManager :: IO Manager
makeManager = newManager tlsManagerSettings

decodeBody :: FromJSON b => Response L.ByteString -> Maybe b
decodeBody = decode . responseBody

-- | Gets a list of available "Symbol"s with their associated details.
getSymbolsDetails :: IO (Either String [Symbol])
getSymbolsDetails = do
    request <- parseUrl "https://api.bitfinex.com/v1/symbols_details"
    m <- makeManager
    a <- httpLbs request m
    let b = decodeBody a
    case b of
        Nothing -> return $ Left "Error: No response body."
        Just x -> return $ Right x

-- | Gets a list of available "Ticker"s.
getSymbols :: IO (Either String [Ticker])
getSymbols = do
    request <- parseUrl "https://api.bitfinex.com/v1/symbols"
    m <- makeManager
    a <- httpLbs request m
    let b = decodeBody a
    case b of
        Nothing -> return $ Left "Error: No response body."
        Just x -> return $ Right x

-- | Get the most recent "TickerData" for a "Ticker". Contains lots of price data.
getTicker :: Ticker -> IO (Either String TickerData)
getTicker t = do
    request <- parseUrl $ "https://api.bitfinex.com/v1/pubticker/" ++ unTicker t
    m <- makeManager
    a <- httpLbs request m
    let b = decodeBody a
    case b of
        Nothing -> return $ Left "Error: No response body or bad parse."
        Just x -> return $ Right x

-- | Get a list of the most recent "Stats" of a "Ticker". Contains volume and period.
getStats :: Ticker -> IO (Either String [Stats])
getStats t = do
    request <- parseUrl $ "https://api.bitfinex.com/v1/stats/" ++ unTicker t
    m <- makeManager
    a <- httpLbs request m
    let b = decodeBody a
    case b of
        Nothing -> return $ Left "Error: No response body or bad parse."
        Just x -> return $ Right x

-- | Get the most recent "FundingBook" of a "Currency".
getFundingBook :: Currency -> IO (Either String FundingBook)
getFundingBook c = do
    request <- parseUrl $ "https://api.bitfinex.com/v1/lendbook/" ++ unCurr c
    m <- makeManager
    a <- httpLbs request m
    let b = decodeBody a
    case b of
        Nothing -> return $ Left "Error: No response body or bad parse."
        Just x -> return $ Right x

-- | Get the most recent "OrderBook" of a "Ticker"
getOrderBook :: Ticker -> IO (Either String OrderBook)
getOrderBook c = do
    request <- parseUrl $ "https://api.bitfinex.com/v1/book/" ++ unTicker c
    m <- makeManager
    a <- httpLbs request m
    let b = decodeBody a
    case b of
        Nothing -> return $ Left "Error: No response body or bad parse."
        Just x -> return $ Right x

-- | Get list of all recent "Trade"s of a "Ticker". Returns an error string if fails.
getTrades :: Ticker -> IO (Either String [Trade])
getTrades t = do
    request <- parseUrl $ "https://api.bitfinex.com/v1/trades/" ++ unTicker t
    m <- makeManager
    a <- httpLbs request m
    let b = decodeBody a
    case b of
        Nothing -> return $ Left "Error: No response body or bad parse."
        Just x -> return $ Right x

-- | Get list of all recent "Loan"s: total amount lent and FRR in % by 365 days.
getLoans :: Currency -> IO (Either String [Loan])
getLoans c = do
    request <- parseUrl $ "https://api.bitfinex.com/v1/lends/" ++ unCurr c
    m <- makeManager
    a <- httpLbs request m
    let b = decodeBody a
    case b of
        Nothing -> return $ Left "Error: No response body or bad parse."
        Just x -> return $ Right x
