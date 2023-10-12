import           Gcd        (euclid1, euclid2)

import           Test.Hspec (context, describe, hspec, it, shouldBe)

-- TODO - use quickcheck to compare results from euclid1, euclid2 and gcd

main :: IO ()
main = hspec $ do

  describe "euclid1" $ do
    context "euclid1 1 1" $
      it "returns 1" $
        euclid1 1 1 `shouldBe` 1
    context "euclid1 371 379904" $
      it "returns 371" $
        euclid1 371 379904 `shouldBe` 371

  describe "euclid2" $ do
    context "euclid2 1 1" $
      it "returns 1" $
        euclid2 1 1 `shouldBe` 1
    context "euclid2 371 379904" $
      it "returns 371" $
        euclid2 371 379904 `shouldBe` 371

  describe "gcd" $ do
    context "gcd 1 1" $
      it "returns 1" $
        gcd 1 1 `shouldBe` 1
    context "gcd 371 379904" $
      it "returns 371" $
        gcd 371 379904 `shouldBe` 371
