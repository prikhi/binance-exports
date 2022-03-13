{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE RecordWildCards #-}
{- | CLI application harness.
-}
module Console.Binance.Exports.Main
    ( run
    , getArgs
    , Args(..)
    ) where

import           Control.Monad.IO.Class         ( liftIO )
import           Data.List                      ( sortOn )
import           Data.Ord                       ( Down(..) )
import           Data.Time                      ( UTCTime(..)
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
import           System.Exit                    ( exitFailure )
import           System.IO                      ( stderr )

import           Console.Binance.Exports.Csv
import           Paths_binance_exports          ( version )
import           Web.Binance

import qualified Data.ByteString.Lazy.Char8    as LBS
import qualified Data.Text                     as T
import qualified Data.Text.IO                  as T


-- | Generate & print a trade export based on the executable arguments.
run :: Args -> IO ()
run Args {..} = do
    results <- runApi cfg $ do
        symbolDetails <-
            fmap eiSymbols
            $   getExchangeInfo (map T.pack symbols)
            >>= handleBinanceError
        rawExportData <- concat <$> mapM getTradesForSymbol symbolDetails
        return . filterYear $ sortOn (Down . tTime . tedTrade) rawExportData
    -- Print CSV to stdout
    LBS.putStr $ buildTradeExport results
  where
    -- | Build a config for the Binance API requests from the CLI
    -- arguments.
    cfg :: BinanceConfig
    cfg = BinanceConfig { bcApiKey    = T.pack apiKey
                        , bcApiSecret = T.pack apiSecret
                        }
    -- | If an error is present, print the code & message to stderr, then
    -- exit with an error status code.
    handleBinanceError :: Either BinanceError a -> BinanceApiM a
    handleBinanceError = \case
        Left e -> liftIO $ do
            T.hPutStrLn stderr
                $  "[ERROR] Binance API Error Code "
                <> T.pack (show $ beCode e)
                <> ": "
                <> beMsg e
            exitFailure
        Right r -> return r
    -- | Get all trades for the given symbol & convert them into the export
    -- format.
    getTradesForSymbol :: SymbolDetails -> BinanceApiM [TradeExportData]
    getTradesForSymbol s =
        map (TradeExportData s) <$> getTradeHistory (sdSymbol s) Nothing Nothing
    -- | Filter the trades if a 'year' argument has been passed.
    filterYear :: [TradeExportData] -> [TradeExportData]
    filterYear = case year of
        Nothing -> id
        Just y ->
            filter
                $ (\(y_, _, _) -> y == y_)
                . toGregorian
                . utctDay
                . posixSecondsToUTCTime
                . tTime
                . tedTrade


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
