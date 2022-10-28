module Main where

import           Lib
import           System.Environment

apiConfigDefault = BinanceApiConfig { 
    apiRoot = "https://api.binance.com",
    apiKey = "",
    apiSecret = ""
}

main :: IO ()
main = do
    apiKey    <- getEnv "BINANCE_API_KEY"
    apiSecret <- getEnv "BINANCE_API_SECRET"
    runApiClient apiConfigDefault { apiKey = apiKey, apiSecret = apiSecret }
