{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleInstances #-}
{-| Request functions & response types for the Binance.US API
-}
module Web.Binance
    (
    -- * Config
      BinanceConfig(..)
    , BinanceApiM
    , runApi
    -- * Requests
    -- ** Exchange Info
    , getExchangeInfo
    , ExchangeInfo(..)
    , SymbolDetails(..)
    -- ** Trade History
    , getTradeHistory
    , Trade(..)
    -- * Helpers
    , runSignedRequest
    , mkSignature
    ) where

import           Control.Monad.Reader           ( ReaderT
                                                , ask
                                                , lift
                                                , liftIO
                                                , runReaderT
                                                )
import           Crypto.Hash.SHA256             ( hmac )
import           Data.Aeson                     ( (.:)
                                                , FromJSON(..)
                                                , withObject
                                                )
import           Data.Function                  ( on )
import           Data.List                      ( minimumBy )
import           Data.Proxy                     ( Proxy )
import           Data.Scientific                ( Scientific )
import           Data.Text.Encoding             ( encodeUtf8 )
import           Data.Time                      ( UTCTime
                                                , getCurrentTime
                                                )
import           Data.Time.Clock.POSIX          ( POSIXTime
                                                , posixSecondsToUTCTime
                                                )
import           Data.Time.Format               ( defaultTimeLocale
                                                , formatTime
                                                )
import           Network.HTTP.Client            ( RequestBody(..)
                                                , queryString
                                                , requestBody
                                                )
import           Network.HTTP.Req               ( (/:)
                                                , (=:)
                                                , AllowsBody
                                                , GET(..)
                                                , HttpBody
                                                , HttpBodyAllowed
                                                , HttpMethod
                                                , HttpResponse
                                                , MonadHttp(..)
                                                , NoReqBody(..)
                                                , Option
                                                , ProvidesBody
                                                , Req
                                                , Url
                                                , defaultHttpConfig
                                                , header
                                                , https
                                                , jsonResponse
                                                , req
                                                , reqCb
                                                , responseBody
                                                , runReq
                                                )
import           Text.Bytedump                  ( hexString )

import qualified Data.ByteString               as BS
import qualified Data.ByteString.Char8         as BC
import qualified Data.ByteString.Lazy          as LBS
import qualified Data.Text                     as T


-- | Necessary configuration data for making requests to the Binance API.
data BinanceConfig = BinanceConfig
    { bcApiKey    :: T.Text
    -- ^ Your API Key
    , bcApiSecret :: T.Text
    -- ^ Your API Key's Secret
    }
    deriving (Show, Read, Eq, Ord)

-- | Run a series of API requests with the given Config.
runApi :: BinanceConfig -> BinanceApiM a -> IO a
runApi cfg = runReq defaultHttpConfig . flip runReaderT cfg

type BinanceApiM = ReaderT BinanceConfig Req

-- | Use 'MonadHttp' from the 'Req' instance.
instance  MonadHttp BinanceApiM where
    handleHttpException = lift . handleHttpException


-- EXCHANGE INFO

-- | Get Exchange Information for the Given Symbol. Right now, just returns
-- the requested symbol information.
getExchangeInfo :: MonadHttp m => [T.Text] -> m ExchangeInfo
getExchangeInfo symbols = do
    let symbolsParam =
            mconcat
                [ "["
                , T.intercalate "," (map (\s -> "\"" <> s <> "\"") symbols)
                , "]"
                ]
    resp <- req GET
                (https "api.binance.us" /: "api" /: "v3" /: "exchangeInfo")
                NoReqBody
                jsonResponse
                ("symbols" =: symbolsParam)
    return $ responseBody resp

newtype ExchangeInfo = ExchangeInfo
    { eiSymbols :: [SymbolDetails]
    } deriving (Show, Read, Eq, Ord)

instance FromJSON ExchangeInfo where
    parseJSON =
        withObject "ExchangeInfo" $ \o -> ExchangeInfo <$> o .: "symbols"

data SymbolDetails = SymbolDetails
    { sdSymbol              :: T.Text
    , sdBaseAsset           :: T.Text
    , sdBaseAssetPrecision  :: Int
    , sdQuoteAsset          :: T.Text
    , sdQuoteAssetPrecision :: Int
    }
    deriving (Show, Read, Eq, Ord)

instance FromJSON SymbolDetails where
    parseJSON = withObject "SymbolDetails" $ \o -> do
        sdSymbol              <- o .: "symbol"
        sdBaseAsset           <- o .: "baseAsset"
        sdBaseAssetPrecision  <- o .: "baseAssetPrecision"
        sdQuoteAsset          <- o .: "quoteAsset"
        sdQuoteAssetPrecision <- o .: "quoteAssetPrecision"
        return SymbolDetails { .. }


-- TRADE HISTORY

-- | Get Trade History for the Given Symbol.
getTradeHistory
    :: MonadHttp m
    => T.Text
    -- ^ Full symbol/pair of trades to fetch, e.g. @BNBUSD@.
    -> Maybe UTCTime
    -- ^ Start of time range
    -> Maybe UTCTime
    -- ^ End of time range
    -> ReaderT BinanceConfig m [Trade]
getTradeHistory symbol mbStart mbEnd = do
    cfg       <- ask
    timestamp <- utcToMs <$> liftIO getCurrentTime
    let limit = (1000 :: Int)
    resp <- runSignedRequest
        GET
        (https "api.binance.us" /: "api" /: "v3" /: "myTrades")
        NoReqBody
        jsonResponse
        (mconcat
            [ "symbol" =: symbol
            , "timestamp" =: timestamp
            , "limit" =: limit
            , maybe mempty (("startTime" =:) . utcToMs) mbStart
            , maybe mempty (("endTime" =:) . utcToMs)   mbEnd
            , header "X-MBX-APIKEY" (encodeUtf8 $ bcApiKey cfg)
            ]
        )
    let results = responseBody resp
    if length results /= limit
        then return results
        else do
            let minTime = minimumBy (compare `on` tTime) results
            (results <>) <$> getTradeHistory
                symbol
                mbStart
                (Just . posixSecondsToUTCTime $ tTime minTime)

-- | A single trade made on Binance.
data Trade = Trade
    { tSymbol          :: T.Text
    -- ^ Full symbol of the trade - base asset & quote asset
    , tId              :: Integer
    -- ^ Trade's ID number
    , tOrderId         :: Integer
    -- ^ Order ID number from which the Trade was made
    , tPrice           :: Scientific
    , tQuantity        :: Scientific
    , tQuoteQuantity   :: Scientific
    -- ^ The total amount spent/received during the trade. Note that we do
    -- not use this value in our exports, as Binance truncates it & loses
    -- a fraction of the amount. You probably want to do @'tQuantity'
    -- * 'tPrice'@ instead.
    , tCommission      :: Scientific
    , tCommissionAsset :: T.Text
    , tTime            :: POSIXTime
    , tIsBuyer         :: Bool
    , tIsMaker         :: Bool
    , tIsBestMatch     :: Bool
    }
    deriving (Show, Read, Eq, Ord)

instance FromJSON Trade where
    parseJSON = withObject "Trade" $ \o -> do
        tSymbol          <- o .: "symbol"
        tId              <- o .: "id"
        tOrderId         <- o .: "orderId"
        tPrice           <- read <$> o .: "price"
        tQuantity        <- read <$> o .: "qty"
        tQuoteQuantity   <- read <$> o .: "quoteQty"
        tCommission      <- read <$> o .: "commission"
        tCommissionAsset <- o .: "commissionAsset"
        -- Binance API returns milliseconds, POSIXTime is seconds
        tTime            <- (/ 1000.0) <$> o .: "time"
        tIsBuyer         <- o .: "isBuyer"
        tIsMaker         <- o .: "isMaker"
        tIsBestMatch     <- o .: "isBestMatch"
        return Trade { .. }


-- UTILS

-- | Run a request for a SIGNED endpoint by inserting the signature before
-- making the request.
runSignedRequest
    :: ( MonadHttp m
       , HttpMethod method
       , HttpBody body
       , HttpResponse response
       , HttpBodyAllowed (AllowsBody method) (ProvidesBody body)
       )
    => method
    -> Url scheme
    -> body
    -> Proxy response
    -> Option scheme
    -> ReaderT BinanceConfig m response
runSignedRequest m u b p s = do
    cfg <- ask
    lift . reqCb m u b p s $ \req_ -> do
        let qs   = BS.drop 1 $ queryString req_
            body = getBodyBS $ requestBody req_
            sig  = mkSignature cfg qs body
            qs_  = if BS.length qs == 0
                then "?signature=" <> sig
                else qs <> "&signature=" <> sig
        return $ req_ { queryString = qs_ }
  where
    getBodyBS = \case
        RequestBodyLBS lbs -> LBS.toStrict lbs
        RequestBodyBS  bs  -> bs
        _                  -> ""

-- | Generate a HMAC SHA256 signature for a SIGNED api request.
mkSignature :: BinanceConfig -> BS.ByteString -> BS.ByteString -> BS.ByteString
mkSignature cfg queryParams reqBody =
    let totalParams = queryParams <> reqBody
        key         = encodeUtf8 $ bcApiSecret cfg
    in  BC.pack . concatMap hexString . BS.unpack $ hmac key totalParams

-- | Convert UTC into posix milliseconds for the Binance API.
utcToMs :: UTCTime -> String
utcToMs = formatTime defaultTimeLocale "%s000"
