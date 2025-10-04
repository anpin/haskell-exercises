module FizzBuzz (fizzBuzz) where

fizzBuzz :: Integer -> [String]
fizzBuzz n =
  [0 .. n]
    & map fb
  where
    fb x
      | x == 0 = show x
      | x `mod` 15 == 0 = "FizzBuzz" -- yikes, I'm lazy
      | x `mod` 5 == 0 = "Buzz"
      | x `mod` 3 == 0 = "Fizz"
      | otherwise = show x
