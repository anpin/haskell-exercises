module BasicsSpec (spec) where

import Basics qualified as B

import Test.Hspec

spec :: Spec
spec = do
  describe "custom basic functions" $ do
    it "map" $ do
      (B.mapX (+ 1) [1, 2, 3])
        `shouldBe` [2, 3, 4]
