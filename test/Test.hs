import           Gcd        (euclid1, euclid2)

import           Bezout     (besout)
import           Test.Hspec (context, describe, hspec, it, shouldBe)

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

  describe "besout" $ do
    context "besout 1 1" $
      it "returns 1" $
        besout 1 1 `shouldBe` [0,1,1]
    context "besout 371 379904" $
      it "returns 371" $
        besout 371 379904 `shouldBe` [1,0,371]
