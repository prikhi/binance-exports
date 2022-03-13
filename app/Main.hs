module Main where

import           Console.Binance.Exports.Main


main :: IO ()
main = getArgs >>= run
