{-# LANGUAGE UnicodeSyntax #-}

module Main (main) where

import           GCD            (euclid1, euclid2)

import           Criterion.Main

main :: IO ()
main = defaultMain
  [ bgroup "euclid 1 subtraction"
    [
      bench "317" $ whnf euclid1 317
    ],
    bgroup "euclid 2 modulus"
    [
      bench "317" $ whnf euclid2 317
    ]
  ]

