{-# LANGUAGE UnicodeSyntax #-}

module Main (main) where

import           GCD            (euclid1, euclid2)

import           Bezout         (besout)
import           Criterion.Main (bench, bgroup, defaultMain, whnf)

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
    ],
    bgroup "besout: "
    [
      bench "379904" $ whnf besout 379904
    ]
  ]

