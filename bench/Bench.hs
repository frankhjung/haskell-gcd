{-# LANGUAGE UnicodeSyntax #-}

module Main (main) where

import           GCD            (euclid1, euclid2)

import           Criterion.Main

main :: IO ()
main = defaultMain

  [
    bgroup "euclid1: "
    [
      bench "379904" $ whnf euclid1 379904
    ],
    bgroup "euclid2: "
    [
      bench "379904" $ whnf euclid2 379904
    ]
  ]

