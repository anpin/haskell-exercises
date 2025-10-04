module TowersOfHanoiSpec (spec) where

import Test.Hspec
import TowersOfHanoi

spec :: Spec
spec = do
  describe "hanoi" $ do
    it "returns correct moves for 2 disks" $ do
      let result = hanoi 2 "a" "b" "c"
      result `shouldBe` [("a", "c"), ("a", "b"), ("c", "b")]
    it "returns correct moves for 3 disks" $ do
      let result = hanoi 3 "a" "b" "c"
      result `shouldBe` [("a", "b"), ("a", "c"), ("b", "c"), ("a", "b"), ("c", "a"), ("c", "b"), ("a", "b")]
