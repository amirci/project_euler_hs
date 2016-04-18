module P054Spec (main, spec) where

import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck
import Test.QuickCheck.Property
import Test.QuickCheck.Modifiers
import Debug.Trace

import Test.Hspec

import Problem054

main :: IO ()
main = hspec spec

spec :: Spec
spec = do

  describe "Calculating a hand" $ do
    context "When there's nothing in the hand" $ do
      it "returns the cards sorted by value, highest first" $ do
        handValue ["5D", "8C", "9S", "JS", "AC"] `shouldBe` "0AJ985"

    context "When there's a single pair" $ do
      it "returns a pair with the rest of the cards ordered" $ do
        handValue ["2C", "3S", "8S", "8D", "TD"] `shouldBe` "108T32"

    context "when there's two pairs" $ do
      it "returns two pairs with the rest of the cards ordered" $ do
        handValue ["5H", "5C", "QS", "QS", "KD"] `shouldBe` "200Q5K"

    context "When there's 3 of a kind" $ do
      it "returns the 3 of a kind card with the other 2 cards sorted H to L" $ do
        handValue ["5H", "5C", "5S", "QS", "TD"] `shouldBe` "3005QT"

    context "When there are consecutive cards not matching suits" $ do
      it "returns all cards sorted H to L" $ do
        handValue ["5H", "4D", "3S", "2S", "6D"] `shouldBe` "465432"

    context "When there's a flush (all same suit)" $ do
      it "returns the cards sorted by value, highest first" $ do
        handValue ["5D", "8D", "9D", "JD", "AD"] `shouldBe` "5AJ985"

    context "When there's a full house" $ do
      it "returns the trio and then the pair" $ do
        handValue ["5D", "5C", "5S", "TS", "TC"] `shouldBe` "60005T"

    context "When there's four of a kind" $ do
      it "returns the 4th and then the other card" $ do
        handValue ["5D", "5C", "5S", "5S", "TC"] `shouldBe` "70005T"

    context "When there are consecutive cards matching suits" $ do
      it "returns all cards sorted H to L" $ do
        handValue ["5D", "4D", "3D", "2D", "6D"] `shouldBe` "865432"

    context "When there are royal cards matching suits" $ do
      it "returns 900000" $ do
        handValue ["AD", "JD", "QD", "TD", "KD"] `shouldBe` "900000"

  describe "Playing hands" $ do

    context "Playing 5H 5C 6S 7S KD vs 2C 3S 8S 8D TD" $ do
      it "P2 Wins" $ do
        playHand "5H 5C 6S 7S KD 2C 3S 8S 8D TD" `shouldBe` Player2

    context "Playing 5D 8C 9S JS AC vs 2C 5C 7D 8S QH" $ do
      it "P1 Wins" $ do
        playHand "5D 8C 9S JS AC 2C 5C 7D 8S QH" `shouldBe` Player1

    context "3 Aces vs Flush of D" $ do
      it "P2 Wins" $ do
        playHand "2D 9C AS AH AC 3D 6D 7D TD QD" `shouldBe` Player2


  describe "Using the test file" $ do
    it "Plays all the hands in the file and counts P1 wins" $ do
      contents <- readFile "test/p054_poker.txt"
      let hands = length $ filter (== Player1) $ map playHand $ lines contents
      hands `shouldBe` 376
