{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE RecordWildCards #-}
{- | CLI application harness.

-}
module Lib.Main
    ( run
    , getArgs
    , Args(..)
    ) where

import           Data.Csv                       ( defaultEncodeOptions
                                                , encUseCrLf
                                                , encodeDefaultOrderedByNameWith
                                                )
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

import           Paths_binance_exports          ( version )
import           Web.Binance

import qualified Data.ByteString.Lazy.Char8    as LBS
import qualified Data.Text                     as T


-- | Generate & print a trade export based on the executable arguments.
run :: Args -> IO ()
run Args {..} = do
    let cfg = BinanceConfig { bcApiKey    = T.pack apiKey
                            , bcApiSecret = T.pack apiSecret
                            }
    results <- filterYear . sortOn (Down . tTime) . concat <$> runApi
        cfg
        (mapM (\(T.pack ->s) -> getTradeHistory s Nothing Nothing) symbols)
    LBS.putStr $ encodeDefaultOrderedByNameWith
        (defaultEncodeOptions { encUseCrLf = False })
        results
  where
    filterYear = case year of
        Nothing -> id
        Just y ->
            filter
                $ (\(y_, _, _) -> y == y_)
                . toGregorian
                . utctDay
                . posixSecondsToUTCTime
                . tTime



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
