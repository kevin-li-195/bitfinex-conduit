{-# LANGUAGE OverloadedStrings #-}

module Bitfinex.Types where

import Data.Aeson
import Data.Ratio
import qualified Data.Scientific as Sci
import Data.Text (unpack)
import Data.Time.Clock
import Data.Time.Format
import Control.Monad
import Control.Applicative

data Symbol = Symbol
    { getSymbolPair :: Ticker
    , getSymbolPrecision :: Int
    , getSymbolInitialMargin :: Price
    , getSymbolMinimumMargin :: Price
    , getSymbolMaxOrderSize :: Price
    , getSymbolMinOrderSize :: Price
    , getSymbolExpiration :: String
    }
    deriving Show

data TickerData = TickerData
    { getTickerMid :: Price
    , getTickerBid :: Price
    , getTickerAsk :: Price
    , getTickerLastPrice :: Price
    , getTickerLow :: Price
    , getTickerHigh :: Price
    , getTickerVolume :: Price
    , getTickerTime :: BTCTime
    }
    deriving Show

-- newtype BTCTime = BTCTime { btcTime :: UTCTime }
--     deriving (Eq, Ord, Show)

data BTCTime = BTCTime Double
    deriving Show

data FundingBook = FundingBook
    { getFundingBids :: [FundingBid]
    , getFundingAsks :: [FundingAsk]
    }
    deriving Show

data TradeType = Buy | Sell
    deriving Show

newtype Exchange = Exchange { unExchange :: String }
    deriving Show

data OrderBook = OrderBook
    { getOrderBids :: [OrderBid]
    , getOrderAsks :: [OrderAsk]
    }
    deriving Show

newtype Price = Price { unPrice :: Double }
    deriving Show

data Currency = Currency { unCurr :: String }
    deriving Show

data Ticker = Ticker { unTicker :: String }
    deriving Show

data Stats = Stats
    { getStatsPeriod :: Int
    , getStatsVolume :: Price
    }
    deriving Show

data FRR = YesFRR | NoFRR
    deriving Show

data FundingBid = FundingBid
    { getFundingBidRate :: Price
    , getFundingBidAmount :: Price
    , getFundingBidPeriod :: Int
    , getFundingBidTimestamp :: BTCTime
    , getFundingBidFrr :: FRR
    }
    deriving Show

data FundingAsk = FundingAsk
    { getFundingAskRate :: Price
    , getFundingAskAmount :: Price
    , getFundingAskPeriod :: Int
    , getFundingAskTimestamp :: BTCTime
    , getFundingAskFrr :: FRR
    }
    deriving Show

data OrderBid = OrderBid
    { getOrderBidRate :: Price
    , getOrderBidAmount :: Price
    , getOrderBidTimestamp :: BTCTime
    }
    deriving Show

data OrderAsk = OrderAsk
    { getOrderAskRate :: Price
    , getOrderAskAmount :: Price
    , getOrderAskTimestamp :: BTCTime
    }
    deriving Show

data Trade = Trade
    { getTradeTimestamp :: BTCTime
    , getTradeID :: Int
    , getTradePrice :: Price
    , getTradeAmount :: Price
    , getTradeExchange :: String
    , getTradeType :: TradeType
    }
    deriving Show

data Loan = Loan
    { getLoanRate :: Double
    , getLoanAmount :: Price
    , getLoanAmountUsed :: Price
    , getLoanTimestamp :: BTCTime
    }
    deriving Show

-- TODO: Combine FromJSON instances of OrderBook and FundingBook along with
-- data constructors of OrderAsk, OrderBid, FundingAsk, FundingBid

-- TODO: Fix dangerous read. Change to reads.

-- TODO: Make BTCTime better
instance FromJSON Loan where
    parseJSON (Object o) = Loan
                    <$> (fmap read $ o .: "rate")
                    <*> o .: "amount_lent"
                    <*> o .: "amount_used"
                    <*> o .: "timestamp"

instance FromJSON Trade where
    parseJSON (Object o) = Trade
                    <$> o .: "timestamp"
                    <*> o .: "tid"
                    <*> o .: "price"
                    <*> o .: "amount"
                    <*> o .: "exchange"
                    <*> o .: "type"
    parseJSON _          = empty

instance FromJSON Exchange where
    parseJSON (String s) = Exchange <$> pure (unpack s)
    parseJSON _          = empty

instance FromJSON TradeType where
    parseJSON (String s) = case s of
                    "buy" -> pure Buy
                    "sell" -> pure Sell
                    _ -> error "Error: Parse failed on TradeType."
    parseJSON _          = empty
    
instance FromJSON OrderBook where
    parseJSON (Object o) = OrderBook
                    <$> o .: "bids"
                    <*> o .: "asks"
    parseJSON _          = empty

instance FromJSON FundingBook where
    parseJSON (Object o) = FundingBook
                    <$> o .: "bids"
                    <*> o .: "asks"
    parseJSON _          = empty

instance FromJSON FundingBid where
    parseJSON (Object o) = FundingBid
                    <$> o .: "rate"
                    <*> o .: "amount"
                    <*> o .: "period"
                    <*> o .: "timestamp"
                    <*> o .: "frr"
    parseJSON _          = empty

instance FromJSON FundingAsk where
    parseJSON (Object o) = FundingAsk
                    <$> o .: "rate"
                    <*> o .: "amount"
                    <*> o .: "period"
                    <*> o .: "timestamp"
                    <*> o .: "frr"
    parseJSON _          = empty

instance FromJSON OrderBid where
    parseJSON (Object o) = OrderBid
                    <$> o .: "price"
                    <*> o .: "amount"
                    <*> o .: "timestamp"
    parseJSON _          = empty

instance FromJSON OrderAsk where
    parseJSON (Object o) = OrderAsk
                    <$> o .: "price"
                    <*> o .: "amount"
                    <*> o .: "timestamp"
    parseJSON _          = empty

instance FromJSON FRR where
    parseJSON (String s) = case s of
                    "Yes" -> pure YesFRR
                    "No" -> pure NoFRR
                    _ -> error "Failed parse on FRR."
    parseJSON _ = empty

-- timefmt = "%s"
-- timeParse = parseTimeM True defaultTimeLocale timefmt
-- instance FromJSON BTCTime where
--     parseJSON (Number n) = BTCTime <$> timeParse (show (numerator $ toRational n))
--     parseJSON _ = empty
instance FromJSON BTCTime where
    parseJSON (String s) = BTCTime <$> pure ((read . unpack) s)
    parseJSON (Number n) = BTCTime <$> pure (Sci.toRealFloat n)
    parseJSON _ = empty

instance FromJSON Price where
    parseJSON (String s) = Price <$> pure ((read . unpack) s)
    parseJSON _ = empty

instance FromJSON Ticker where
    parseJSON (String s) = Ticker <$> pure (unpack s)
    parseJSON _ = empty

instance FromJSON Stats where
    parseJSON (Object o) = Stats
                           <$> o .: "period"
                           <*> o .: "volume"
    parseJSON _ = empty

-- TODO: Finish period parsing (value constructor of Period incorrect?)
-- instance FromJSON Period where
--     parseJSON (Number n) = do
--                         let num = Sci.toBoundedInteger n
--                         case num of
--                             Nothing -> pure 0
--                             Just x -> pure x
-- 
-- instance FromJSON UTCTime where
--     parseJSON (Number d) = UTCTime <$> parseTimeM True defaultTimeLocale "%s" $
--                     show $ Sci.toRealFloat d
--     parseJSON _ = mzero

instance FromJSON TickerData where
    parseJSON (Object o) = TickerData
                           <$> o .: "mid"
                           <*> o .: "bid"
                           <*> o .: "ask"
                           <*> o .: "last_price"
                           <*> o .: "low"
                           <*> o .: "high"
                           <*> o .: "volume"
                           <*> o .: "timestamp"
    parseJSON _ = empty

instance FromJSON Symbol where
    parseJSON (Object o) = Symbol <$>
                           o .: "pair" <*>
                           o .: "price_precision" <*>
                           o .: "initial_margin" <*>
                           o .: "minimum_margin" <*>
                           o .: "maximum_order_size" <*>
                           o .: "minimum_order_size" <*>
                           o .: "expiration"
    parseJSON _          = empty
