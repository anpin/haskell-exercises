module CreditCardSpec (spec) where

import CreditCard
import Data.Vector qualified as V
import Test.Hspec

spec :: Spec
spec = do
  describe "parseCreditCardVector" $ do
    it "parses valid credit card number" $ do
      let result = parseCreditCardVector "4012888888881881"
      result `shouldBe` Right (CreditCardNumber (V.fromList [4, 0, 1, 2, 8, 8, 8, 8, 8, 8, 8, 8, 1, 8, 8, 1]))

    it "filters out non-digit characters" $ do
      let result = parseCreditCardVector "4012-8888-8888-1881"
      result `shouldBe` Right (CreditCardNumber (V.fromList [4, 0, 1, 2, 8, 8, 8, 8, 8, 8, 8, 8, 1, 8, 8, 1]))

    it "handles empty string" $ do
      let result = parseCreditCardVector ""
      result `shouldSatisfy` isLeft

  describe "validateCreditCard" $ do
    it "validates correct credit card number (example from exercise)" $ do
      let result = parseCreditCardVector "4012888888881881" >>= validateCreditCard
      result `shouldSatisfy` isRight

    it "rejects invalid credit card number" $ do
      let result = parseCreditCardVector "4012888888881882" >>= validateCreditCard
      result `shouldSatisfy` isLeft

    it "rejects credit card number with invalid length (too short)" $ do
      let result = parseCreditCardVector "123" >>= validateCreditCard
      result `shouldSatisfy` isLeft

    it "rejects credit card number with invalid length (too long)" $ do
      let result = parseCreditCardVector "12345678901234567890" >>= validateCreditCard
      result `shouldSatisfy` isLeft

    it "validates 14-digit card" $ do
      let result = parseCreditCardVector "38520000023237" >>= validateCreditCard
      result `shouldSatisfy` isRight
