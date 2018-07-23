module Main where

import           System.Environment (getArgs)

import           GCD                (euclid, euclid')

-- Read two numbers from command line to evaluate their greatest common
-- denominator.
-- Prints out results from both algorithms: euclid and euclid'

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
      2 -> print $ zipWith3 id [euclid, euclid'] [u,u] [v,v]
            where [u, v] = map read args
      _ -> putStrLn usage
