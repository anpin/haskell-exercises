module CreditCard (
  CreditCardNumber (..),
  CreditCardNumberHash (..),
  parseCreditCardVector,
  validateCreditCard,
) where

import Data.Char (digitToInt, isDigit)
import Data.Vector qualified as V

newtype CreditCardNumber = CreditCardNumber (V.Vector Int)
  deriving newtype (Show, Eq)

newtype CreditCardNumberHash = CreditCardNumberHash Int
  deriving newtype (Show, Eq)

parseCreditCardVector :: [Char] -> Either String CreditCardNumber
parseCreditCardVector = \case
  [] -> Left "Empty credit card number, did you forgot to type?"
  xs -> Right $ CreditCardNumber . V.fromList . map digitToInt . filter isDigit $ xs

validateCreditCard :: CreditCardNumber -> Either String (CreditCardNumber, CreditCardNumberHash)
validateCreditCard (CreditCardNumber v) =
  validLen v
    >>= validHash
  where
    validLen :: V.Vector Int -> Either String (V.Vector Int)
    validLen ve =
      case V.length ve of
        x | x > 13 && x < 19 -> Right ve
        x -> Left ("Invalid credit card number length: " <> show x)

    validHash :: V.Vector Int -> Either String (CreditCardNumber, CreditCardNumberHash)
    validHash ve =
      V.foldr step (0, []) ve
        & \(_, hash) ->
          sumDigits hash
            & \case
              s | s `mod` 10 == 0 -> Right (CreditCardNumber v, CreditCardNumberHash s)
              s -> Left ("Invalid credit card number hash: " <> show s)
      where
        step :: Int -> (Int, [Int]) -> (Int, [Int])
        step x (count, hash)
          | count `mod` 2 == 1 = (count + 1, x * 2 : hash)
          | otherwise = (count + 1, x : hash)
        toDigits :: Int -> [Int]
        toDigits 0 = []
        toDigits x =
          x `mod` 10 : toDigits (x `div` 10)
        sumDigits :: [Int] -> Int
        sumDigits d =
          concatMap toDigits d
            & sum
