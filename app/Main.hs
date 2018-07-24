module Main where

import           System.Environment (getArgs)

import           GCD                (euclid1, euclid2)

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
      2 -> print $ zipWith3 id [euclid1, euclid2] us vs
            where
              [u, v] = map read args
              us = replicate 2 u
              vs = replicate 2 v
      _ -> putStrLn usage
