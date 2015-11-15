{-# LANGUAGE OverloadedStrings #-}

module Bitfinex.Core where

import Network.HTTP.Conduit
import Data.Default
import Data.Aeson
import Bitfinex.Types
import Control.Monad
import Control.Applicative

defaults = def
        { secure = True
        , host = "http://api.bitfinex.com/v1"
        , port = 80 
        }

getSymbols :: IO (Either String [Symbol])
getSymbols = do
    request <- parseUrl "https://api.bitfinex.com/v1/symbols_details"
    manager <- newManager tlsManagerSettings
    a <- httpLbs request manager
    --a <- withManager $ httpLbs $ request
                -- defaults {path = "/symbols_details/"}
    let b = responseBody a
    let c = decode b
    case c of
        Nothing -> return $ Left "Error: No response body."
        Just x -> return $ Right x
