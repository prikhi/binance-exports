{-# LANGUAGE DeriveDataTypeable #-}
{- | CLI application harness.

-}
module Lib.Main
    ( run
    , getArgs
    , Args(..)
    ) where

import           Data.Version                   ( showVersion )
import           System.Console.CmdArgs         ( (&=)
                                                , Data
                                                , Typeable
                                                , cmdArgs
                                                , help
                                                , helpArg
                                                , name
                                                , program
                                                , summary
                                                )

import           Paths_binance_exports          ( version )


-- | Run the executable.
run :: Args -> IO ()
run Args{} = putStrLn "hello world"


-- | CLI arguments supported by the executable.
data Args = Args {}
    deriving (Show, Read, Eq, Data, Typeable)


-- | Parse the CLI arguments with 'System.Console.CmdArgs'.
getArgs :: IO Args
getArgs = cmdArgs argSpec


-- | Defines & documents the CLI arguments.
argSpec :: Args
argSpec =
    Args{}
        &= summary
               (  "binance-exports v"
               <> showVersion version
               <> ", Pavan Rikhi 2022"
               )
        &= program "binance-exports"
        &= helpArg [name "h"]
        &= help "Export Binance Trade History to a CSV"
