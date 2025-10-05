{-# OPTIONS_GHC -Wno-type-defaults #-}

module FibonacciStreamSpec (spec) where

import FibonacciStream
import System.Timeout (timeout)
import Test.Hspec

spec :: Spec
spec = do
  describe "fibonacciStream" $ do
    it "returns the first 10 natural numbers" $ do
      -- this wold fail on timeout and prevent infinite eval
      -- eval works in repl but hangs here in the test, not sure why, hance the delay
      mr <- timeout 3000 $ evaluateNF (streamTakeN 10 nats)
      mr `shouldBe` Just [0 .. 9]
