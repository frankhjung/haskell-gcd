module Main where

import           System.Environment (getArgs)

import           Data.Version       (showVersion)
import           Gcd                (euclid1, euclid2)
import           Paths_gcd          (version)

-- | MAIN
--
-- Read two numbers from command line to evaluate their greatest common
-- denominator.
-- Prints out results from both algorithms: euclid1 and euclid2

-- read version from cabal configuration

usage :: [String]
usage = [
          "Usage: gcd [int] [int]"
        , "Calculates greatest common denominator of two integers."
        , "GCD is caluculated using two different algorithms."
        , "Both are evaluated with results printed on new lines."
        , "See https://en.wikipedia.org/wiki/Euclidean_algorithm."
        , "Version: " ++ showVersion version
        ]

--
-- MAIN
--
main :: IO ()
main = do
    args <- getArgs
    case length args of
      2 ->  mapM_ print ([euclid1, euclid2] <*> [u] <*> [v])
              where [u, v] = map read args
      _ ->  putStrLn $ unlines usage

