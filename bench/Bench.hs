{-# LANGUAGE UnicodeSyntax #-}

module Main (main) where

import           GCD            (euclid, euclid')

import           Criterion.Main

main :: IO ()
main = defaultMain
  [ bgroup "euclid subtraction"
    [
      bench "100" $ whnf euclid 100
    ],
    bgroup "euclid modulus"
    [
      bench "100" $ whnf euclid' 100
    ]
  ]

