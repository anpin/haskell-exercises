module FizzBuzzSpec (spec) where

import FizzBuzz
import Test.Hspec

spec :: Spec
spec = do
  describe "fizzBuzz" $ do
    it "returns 'Fizz' for multiples of 3" $ do
      fizzBuzz 3 `shouldBe` ["0", "1", "2", "Fizz"]
    it "returns 'Buzz' for multiples of 5" $ do
      fizzBuzz 5 `shouldBe` ["0", "1", "2", "Fizz", "4", "Buzz"]
    it "returns 'FizzBuzz' for multiples of 15" $ do
      fizzBuzz 15 `shouldBe` ["0", "1", "2", "Fizz", "4", "Buzz", "Fizz", "7", "8", "Fizz", "Buzz", "11", "Fizz", "13", "14", "FizzBuzz"]
