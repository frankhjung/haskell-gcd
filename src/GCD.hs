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

module GCD (euclid1, euclid2) where

-- | Greatest Common Denominator (for numbers greater than 0)

-- | Method 1 - using only subtraction
euclid1 :: Int -> Int -> Int
euclid1 u v | u <= 0    = 0
            | v <= 0    = 0
            | u == v    = u
            | u < v     = euclid1 u (v - u)
            | otherwise = euclid1 (u - v)  v

-- | Method 2 - using modulus
euclid2 :: Int -> Int -> Int
euclid2 u v = if remainder == 0
                then v
                else euclid2 v remainder
              where remainder = u `mod` v
