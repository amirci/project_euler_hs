module P054Spec (main, spec) where

import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck
import Test.QuickCheck.Property
import Test.QuickCheck.Modifiers

import Test.Hspec

import Problem054

main :: IO ()
main = hspec spec

spec :: Spec
spec = do

  describe "Calculating a hand" $ do
    it "returns a pair with the rest of the cards ordered" $ do
      handValue ["5H", "5C", "6S", "7S", "KD"] `shouldBe` "105K76"

    it "returns two pairs with the rest of the cards ordered" $ do
      handValue ["5H", "5C", "QS", "QS", "KD"] `shouldBe` "200Q5K"

    it "returns three of a kind" $ do
      handValue ["5H", "5C", "5S", "QS", "TD"] `shouldBe` "3005QT"

    context "When there's a full house" $ do
      it "returns the trio and then the pair" $ do
        handValue ["5D", "5C", "5S", "TS", "TC"] `shouldBe` "60005T"

    context "When there's four of a kind" $ do
      it "returns the 4th and then the other card" $ do
        handValue ["5D", "5C", "5S", "5S", "TC"] `shouldBe` "70005T"

    context "When there's nothing in the hand" $ do
      it "returns the cards sorted by value, highest first" $ do
        handValue ["5D", "8C", "9S", "JS", "AC"] `shouldBe` "AJ985"

  describe "Playing hands" $ do
    let h1 = ["5H", "5C", "6S", "7S", "KD"] 
    let h2 = ["2C", "3S", "8S", "8D", "TD"]
    it "returns p1 won" $ do
      playHand h1 h2 `shouldBe` Player1


