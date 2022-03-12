{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE RecordWildCards #-}
{- | CLI application harness.

-}
module Lib.Main
    ( run
    , getArgs
    , Args(..)
    ) where

import           Control.Monad                  ( forM_ )
import           Control.Monad.IO.Class         ( liftIO )
import           Data.Csv                       ( (.=)
                                                , DefaultOrdered(..)
                                                , ToNamedRecord(..)
                                                , defaultEncodeOptions
                                                , encUseCrLf
                                                , encodeDefaultOrderedByNameWith
                                                , header
                                                , namedRecord
                                                )
import           Data.List                      ( sortOn )
import           Data.Ord                       ( Down(..) )
import           Data.Scientific                ( FPFormat(..)
                                                , Scientific
                                                , formatScientific
                                                , isInteger
                                                )
import           Data.Time                      ( UTCTime(..)
                                                , defaultTimeLocale
                                                , formatTime
                                                , toGregorian
                                                )
import           Data.Time.Clock.POSIX          ( posixSecondsToUTCTime )
import           Data.Version                   ( showVersion )
import           System.Console.CmdArgs         ( (&=)
                                                , Data
                                                , Typeable
                                                , args
                                                , cmdArgs
                                                , def
                                                , explicit
                                                , help
                                                , helpArg
                                                , name
                                                , program
                                                , summary
                                                , typ
                                                )
import           System.IO                      ( stderr )

import           Paths_binance_exports          ( version )
import           Web.Binance

import qualified Data.ByteString.Lazy.Char8    as LBS
import qualified Data.List                     as L
import qualified Data.Text                     as T
import qualified Data.Text.IO                  as T


-- | Generate & print a trade export based on the executable arguments.
run :: Args -> IO ()
run Args {..} = do
    results <- runApi cfg $ do
        symbolDetails <- eiSymbols <$> getExchangeInfo (map T.pack symbols)
        -- Log any symbol args not returned by binance.
        -- TODO: This is pointless as 'getExchangeInfo' throws a 400 status
        -- HttpException with a response body code field of -1121 when
        -- asking for invalid symbols.
        liftIO . forM_ symbols $ \(T.pack -> inputSymbol) -> do
            case L.find ((== inputSymbol) . sdSymbol) symbolDetails of
                Nothing ->
                    T.hPutStrLn stderr
                        $  "[ERROR] Binance did not recognize symbol: "
                        <> inputSymbol
                Just _ -> return ()
        rawExportData <- concat <$> mapM getTradesForSymbol symbolDetails
        return . filterYear $ sortOn (Down . tTime . edTrade) rawExportData
    -- Print CSV to stdout
    LBS.putStr $ encodeDefaultOrderedByNameWith
        (defaultEncodeOptions { encUseCrLf = False })
        results
  where
    -- | Build a config for the Binance API requests from the CLI
    -- arguments.
    cfg :: BinanceConfig
    cfg = BinanceConfig { bcApiKey    = T.pack apiKey
                        , bcApiSecret = T.pack apiSecret
                        }
    -- | Get all trades for the given symbol & convert them into the export
    -- format.
    getTradesForSymbol :: SymbolDetails -> BinanceApiM [ExportData]
    getTradesForSymbol s =
        map (ExportData s) <$> getTradeHistory (sdSymbol s) Nothing Nothing
    -- | Filter the trades if a 'year' argument has been passed.
    filterYear :: [ExportData] -> [ExportData]
    filterYear = case year of
        Nothing -> id
        Just y ->
            filter
                $ (\(y_, _, _) -> y == y_)
                . toGregorian
                . utctDay
                . posixSecondsToUTCTime
                . tTime
                . edTrade


-- CSV EXPORT

-- | We need both the 'SymbolDetails' & the 'Trade' to generate an export
-- line.
data ExportData = ExportData
    { edSymbol :: SymbolDetails
    , edTrade  :: Trade
    }
    deriving (Show, Read, Eq, Ord)

-- | We match the format of the old @Trade History@ export as much as
-- possible, but use the asset precisions for the @price@ & @quantity@
-- fields & output the @trade-id@ as well.
instance ToNamedRecord ExportData where
    toNamedRecord (ExportData SymbolDetails {..} Trade {..}) = namedRecord
        [ "time" .= formatTime defaultTimeLocale
                               "%F %T"
                               (posixSecondsToUTCTime tTime)
        , "base-asset" .= sdBaseAsset
        , "quote-asset" .= sdQuoteAsset
        , "type" .= if tIsBuyer then "BUY" else ("SELL" :: String)
        , "price" .= formatScientific Fixed (Just sdQuoteAssetPrecision) tPrice
        , "quantity"
            .= formatScientific Fixed (Just sdBaseAssetPrecision) tQuantity
        , "total" .= renderScientific (tPrice * tQuantity)
        , "fee" .= renderScientific tCommission
        , "fee-currency" .= tCommissionAsset
        , "trade-id" .= tId
        ]
      where
        -- | Render as an integer if possible, otherwise render to the
        -- minimum decimal precision needed.
        renderScientific :: Scientific -> String
        renderScientific p = if isInteger p
            then formatScientific Fixed (Just 0) p
            else formatScientific Fixed Nothing p

instance DefaultOrdered ExportData where
    headerOrder _ = header
        [ "time"
        , "base-asset"
        , "quote-asset"
        , "type"
        , "price"
        , "quantity"
        , "total"
        , "fee"
        , "fee-currency"
        , "trade-id"
        ]


-- CLI ARGS

-- | CLI arguments supported by the executable.
data Args = Args
    { apiKey    :: String
    , apiSecret :: String
    , symbols   :: [String]
    , year      :: Maybe Integer
    }
    deriving (Show, Read, Eq, Data, Typeable)


-- | Parse the CLI arguments with 'System.Console.CmdArgs'.
getArgs :: IO Args
getArgs = cmdArgs argSpec


-- | Defines & documents the CLI arguments.
argSpec :: Args
argSpec =
    Args
            { apiKey    = def
                          &= explicit
                          &= name "k"
                          &= name "api-key"
                          &= help "Binance API Key"
                          &= typ "KEY"
            , apiSecret = def
                          &= explicit
                          &= name "s"
                          &= name "api-secret"
                          &= help "Binance API Secret"
                          &= typ "SECRET"
            , year      = Nothing
                          &= explicit
                          &= name "y"
                          &= name "year"
                          &= help "Limit output to year"
                          &= typ "YYYY"
            , symbols   = def &= args &= typ "SYMBOL [SYMBOL ...]"
            }
        &= summary
               (  "binance-exports v"
               <> showVersion version
               <> ", Pavan Rikhi 2022"
               )
        &= program "binance-exports"
        &= helpArg [name "h"]
        &= help "Export Binance Trade History to a CSV"
