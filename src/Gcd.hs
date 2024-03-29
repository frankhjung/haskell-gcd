{-# LANGUAGE UnicodeSyntax #-}

{-|

  Module      : Gcd
  Description : Show greatest common denominator using Euclid's alogrithm.
  Copyright   : © Frank Jung, 2018-2019
  License     : GPL-3.0-only
  Maintainer  : frankhjung@linux.com
  Stability   : educational
  Portability : Linux

-}

module Gcd (euclid1, euclid2) where

import           Data.Bool (bool)

-- | Greatest Common Denominator (for numbers greater than 0)

-- | Method 1 - using only subtraction
euclid1 :: Word -> Word -> Word
euclid1 u v | u <= 0    = v
            | v <= 0    = u
            | u == v    = u
            | u < v     = euclid1 u (v - u)
            | otherwise = euclid1 (u - v)  v

-- | Method 2 - using modulus
euclid2 :: Word -> Word -> Word
euclid2 u v = bool (euclid2 v remainder) v (remainder == 0)
              where remainder = bool (u `mod` v) u (v == 0)
