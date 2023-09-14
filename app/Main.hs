module Main where

import           Data.Version       (showVersion)
import           Gcd                (euclid1, euclid2)
import           Paths_gcd          (version)
import           System.Environment (getArgs)

-- | MAIN
--
-- Read two numbers from command line to evaluate their greatest common
-- denominator.
-- Prints out results from both algorithms: euclid1 and euclid2

-- read version from cabal configuration

usage :: [String]
usage =
  [ "Usage: gcd [int] [int]",
    "Calculates greatest common denominator of two integers.",
    "GCD is caluculated using two different algorithms.",
    "Both are evaluated with results printed on new lines.",
    "See https://en.wikipedia.org/wiki/Euclidean_algorithm.",
    "Version: " ++ showVersion version
  ]

doEuclids :: [Word] -> IO ()
doEuclids [u, v] = mapM_ print ([euclid1, euclid2] <*> [u] <*> [v])
doEuclids _      = putStrLn $ unlines usage

--
-- MAIN
--
main :: IO ()
main = do
  args <- getArgs
  case length args of
    2 -> let as = map read args
         in doEuclids as
    _ -> putStrLn $ unlines usage
