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

-- | Run using two different algorithms, read values from STDIN.
-- Can we use an arrow here?
doEuclids :: Word -> Word -> IO ()
doEuclids u v = mapM_ print ([euclid1, euclid2] <*> [u] <*> [v])

--
-- MAIN
--
main :: IO ()
main = do
  args <- getArgs
  let as = map read args :: [Word]
  case as of
    [x, y] -> doEuclids x y
    _      -> putStrLn $ unlines usage
