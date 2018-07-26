import           GCD        (euclid1, euclid2)

import           Test.Hspec (context, describe, hspec, it, shouldBe)

main :: IO ()
main = hspec $ do

  describe "euclid1" $ do
    context "euclid1 1 1" $
      it "returns 1" $
        euclid1 1 1 `shouldBe` (1 :: Int)
    context "euclid1 371 379904" $
      it "returns 371" $
        euclid1 371 379904 `shouldBe` (371 :: Int)

  describe "euclid2" $ do
    context "euclid2 1 1" $
      it "returns 1" $
        euclid2 1 1 `shouldBe` (1 :: Int)
    context "euclid2 371 379904" $
      it "returns 371" $
        euclid2 371 379904 `shouldBe` (371 :: Int)
