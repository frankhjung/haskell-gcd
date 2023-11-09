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

-- | Usage information.
usage :: [String]
usage =
  [ "Usage: gcd [int] [int]",
    "Calculates greatest common denominator of two integers.",
    "GCD is caluculated using two different algorithms.",
    "Both are evaluated with results printed on new lines.",
    "See https://en.wikipedia.org/wiki/Euclidean_algorithm.",
    "Version: " ++ showVersion version
  ]

-- | Parse command line arguments.
parseArgs :: [Word] -> Either String (Word, Word)
parseArgs [x, y] = Right (x, y)
parseArgs _      = Left $ unlines usage

-- | Run using two different algorithms.
doEuclids :: (Word, Word) -> String
doEuclids (u, v) = unlines $ map (\f -> show $ f u v) [euclid1, euclid2]

-- | MAIN
-- Process command line arguments and print results.
main :: IO ()
main = putStrLn . either id doEuclids . parseArgs . map read =<< getArgs
