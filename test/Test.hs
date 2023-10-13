{-# LANGUAGE ScopedTypeVariables #-}

import           Gcd                   (euclid1, euclid2)

import           Test.Hspec            (context, describe, hspec, it, shouldBe)
import           Test.Hspec.QuickCheck (prop)

main :: IO ()
main = hspec $ do

  describe "euclid1" $ do
    context "euclid1 1 1 == 1" $
      it "returns 1" $
        euclid1 1 1 `shouldBe` 1
    context "euclid1 371 379904 == 371" $
      it "returns 371" $
        euclid1 371 379904 `shouldBe` 371
    prop "euclid1 x y == gcd x y" $
      \(x, y) -> euclid1 x y `shouldBe` gcd x y

  describe "euclid2" $ do
    context "euclid2 1 1 == 1" $
      it "returns 1" $
        euclid2 1 1 `shouldBe` 1
    context "euclid2 371 379904 == 371" $
      it "returns 371" $
        euclid2 371 379904 `shouldBe` 371
    prop "euclid2 x y == gcd x y" $
      \(x, y) -> euclid2 x y `shouldBe` gcd x y

  describe "gcd" $ do
    context "gcd 1 1" $
      it "returns 1" $
        gcd 1 1 `shouldBe` 1
    context "gcd 371 379904" $
      it "returns 371" $
        gcd 371 379904 `shouldBe` 371
    prop "gcd x y == gcd y x" $
      \(x :: Word, y :: Word) -> gcd x y `shouldBe` gcd y x
