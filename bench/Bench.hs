{-# LANGUAGE UnicodeSyntax #-}

module Main (main) where

import           Gcd            (euclid1, euclid2)

import           Criterion.Main (bench, bgroup, defaultMain, whnf)

main :: IO ()
main = defaultMain

  [
    bgroup "euclid1: " -- algorithm 1
    [
      bench "379904" $ whnf euclid1 379904
    ],
    bgroup "euclid2: " -- algorithm 2
    [
      bench "379904" $ whnf euclid2 379904
    ],
    bgroup "gcd: " -- system function
    [
      bench "379904" $ whnf gcd 379904
    ]
  ]

