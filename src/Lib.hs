{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedStrings #-}

module Lib
    ( runApiClient
    , BinanceApiConfig(..)
    )
where

import           GHC.Generics
import           Data.Aeson
import           Data.Aeson.Lens (_String)
import           Data.Scientific
import           Network.Wreq
import           Control.Lens
import           Data.ByteString.UTF8 (fromString)

apiErrorMsgByCode =
    [ (-1000, "Unknown")
    , (-1001, "Disconnected")
    , (-1002, "Unauthorized")
    , (-1003, "TooManyRequests")
    , (-1006, "UnexpectedResp")
    , (-1007, "Timeout")
    , (-1013, "InvalidMessage")
    , (-1014, "UnknownOrderComposition")
    , (-1015, "TooManyOrders")
    , (-1016, "ServiceShuttingDown")
    , (-1020, "UnsupportedOperation")
    , (-1021, "InvalidTimestamp")
    , (-1022, "InvalidSignature")
    , (-1100, "IllegalChars")
    , (-1101, "TooManyParameters")
    , (-1102, "MandatoryParamEmptyOrMalformed")
    , (-1103, "UnknownParam")
    , (-1004, "UnreadParameters")
    , (-1105, "ParamEmpty")
    , (-1006, "ParamNotRequired")
    , (-1112, "NoDepth")
    , (-1114, "TifNotRequired")
    , (-1115, "InvaliddTif")
    , (-1116, "InvalidOrderType")
    , (-1117, "InvalidSide")
    , (-1118, "EmptyNewClOrdId")
    , (-1119, "EmptyOrgClOrdId")
    , (-1120, "BadInterval")
    , (-1121, "BadSymbol")
    , (-1125, "InvalidListenKey")
    , (-1127, "MoreThanXxHours")
    , (-1128, "OptionalParamsBadCombo")
    , (-1130, "InvalidParameter")
    , (-2008, "BadApiId")
    , (-2009, "DuplicateApiKeyDesc")
    , (-2012, "CancelAllFail")
    , (-2013, "NoSuchOrder")
    , (-2014, "BadApiKeyFmt")
    , (-2015, "RejectedMbxKey")
    ]

lookupApiErrorMsgByCode :: Integer -> Maybe String
lookupApiErrorMsgByCode = flip lookup apiErrorMsgByCode

data ApiResponseFailure = ApiResponseFailure {
    code :: Int,
    message :: String
} deriving (Generic, FromJSON, Show)

data BinanceApiConfig = BinanceApiConfig {
    apiRoot :: String,
    apiKey :: String,
    apiSecret :: String
} deriving (Show, Generic)

newtype Symbol = Symbol String deriving (Generic, FromJSON, Show)
newtype Price = Price String deriving (Generic, FromJSON, Show)
data SymbolPrice = SymbolPrice {
    symbol :: Symbol,
    price :: Price
} deriving (Generic, FromJSON, Show)
newtype Symbols = Symbols [SymbolPrice] deriving (Generic, FromJSON, Show)

data Tickers = Success Symbols | Failure ApiResponseFailure deriving (Generic, FromJSON, Show)
type TickerResponse = Response Symbols

getTickerPrices :: BinanceApiConfig -> IO Symbols
getTickerPrices apiClientConfig = do
    let opts = defaults & header "X-MBS-APIKEY" .~ [fromString (apiKey apiClientConfig)]
    r <- asJSON =<< getWith opts (apiRoot apiClientConfig ++ "/api/v3/ticker/price") :: IO (Response Symbols)
    return $ r ^. responseBody

mapTickerPrices :: Symbols -> [(Symbol, Price)]
mapTickerPrices symbols = []

runApiClient :: BinanceApiConfig -> IO ()
runApiClient apiClientConfig = do
    r <- getTickerPrices apiClientConfig
    print r
