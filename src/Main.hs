{-# LANGUAGE LambdaCase #-}
-- cli tool to solve https://www.seas.upenn.edu/~cis1940/spring13/hw/01-intro.pdf
-- we are using Relude instead of base
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}
-- allow usage of traceShowId
{-# OPTIONS_GHC -fno-warn-deprecations #-}

module Main where

-- import Data.Aeson.Types qualified as Int

import Data.Char (digitToInt, isDigit)
import Data.Vector qualified as V
import Main.Utf8 qualified as Utf8

-- import Numeric

newtype CreditCardNumber = CreditCardNumber (V.Vector Int)
  deriving newtype (Show)
newtype CreditCardNumberHash = CreditCardNumberHash Int
  deriving newtype (Show)
exit = exitSuccess
exitF = exitFailure

parse :: [String] -> IO ()
parse ["-h"] = usage >> exit
parse ["-v"] = version >> exit
parse ["-n", arg] =
  arg
    & parseCreditCardVector
    & traceShowId
    & validateCreditCard
    & \case
      Left err -> putStrLn err >> exitF
      Right (ccn, hash) -> putStrLn ("Valid credit card number: " <> show ccn <> " with hash: " <> show hash) >> exit
parse _ =
  usage >> exitF
usage = putStrLn "Usage: just run [-n] [credit card number]"
version = putStrLn "Haskell exercise #1"

parseCreditCardVector :: [Char] -> CreditCardNumber
parseCreditCardVector =
  CreditCardNumber . V.fromList . map digitToInt . filter isDigit

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

main :: IO ()
main = do
  -- For withUtf8, see https://serokell.io/blog/haskell-with-utf8
  Utf8.withUtf8 $
    do
      putTextLn "Welcome to exercises in Haskell!"
      getArgs >>= parse
