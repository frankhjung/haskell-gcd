module Main where

import           System.Environment (getArgs)

import           GCD                (euclid1, euclid2)

-- Read two numbers from command line to evaluate their greatest common
-- denominator.
-- Prints out results from both algorithms: euclid1 and euclid2

usage :: String
usage = "Usage: gcd [int] [int]" ++
        "\nCalculates greatest common denominator of two integers."

--
-- MAIN
--
main :: IO ()
main = do
    args <- getArgs
    case length args of
      2 -> print $ [euclid1, euclid2] <*> [u] <*> [v]
            where [u, v] = map read args
      _ -> putStrLn usage
