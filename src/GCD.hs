{-# LANGUAGE UnicodeSyntax #-}

{-|

  Module      : GCD
  Description : Show greatest common denominator using Euclid's alogrithm.
  Copyright   : © Frank Jung, 2018
  License     : GPL-3
  Maintainer  : frankhjung@linux.com
  Stability   : experimental
  Portability : Linux

-}

module GCD (euclid, euclid') where

-- | Greatest Common Denominator

-- | Method 1 - using only subtraction
euclid :: Int -> Int -> Int
euclid u v  | u < 0     = 0
            | v < 0     = 0
            | u == v    = u
            | u < v     = euclid u (v - u)
            | otherwise = euclid (u - v)  v

-- | Method 2 - using modulus
euclid' :: Int -> Int -> Int
euclid' u v = if remainder == 0
                then v
                else euclid' v remainder
              where remainder = u `mod` v
