{-# OPTIONS_GHC -Wno-missing-signatures #-}
-- allow usage of traceShowId
{-# OPTIONS_GHC -fno-warn-deprecations #-}

-- cli tool to solve https://www.seas.upenn.edu/~cis1940/spring13/ homework
-- we are using Relude instead of base
module Main where

import CreditCard (parseCreditCardVector, validateCreditCard)
import Main.Utf8 qualified as Utf8

exit = exitSuccess
exitF = exitFailure

parse :: [String] -> IO ()
parse ["-h"] = usage >> exit
parse ["-v"] = version >> exit
parse ["-n", arg] =
  arg
    & parseCreditCardVector
    & traceShowId
    >>= validateCreditCard
      & \case
        Left err -> putStrLn err >> exitF
        Right (ccn, hash) -> putStrLn ("Valid credit card number: " <> show ccn <> " with hash: " <> show hash) >> exit
parse _ =
  usage >> exitF

usage = putStrLn "Usage: just run [-n] [credit card number]"
version = putStrLn "Haskell exercise #1"

main :: IO ()
main = do
  -- For withUtf8, see https://serokell.io/blog/haskell-with-utf8
  Utf8.withUtf8 $
    do
      putTextLn "Welcome to exercises in Haskell!"
      getArgs >>= parse
