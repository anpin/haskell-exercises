{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-top-binds #-}

-- https://www.seas.upenn.edu/~cis1940/spring13/hw/06-laziness.pdf
module FibonacciStream (
  nats,
  -- ruller,
  -- fibs,
  -- streamToList,
  streamTakeN,
) where

import Text.Show qualified -- see migration guide #6 https://hackage.haskell.org/package/relude

{-
 data is like a record type in F#,
 so not a type-allias as newtype,
 this below is a recusrive type definition
 in F# you could write this as
 type Stream<'a> = Cons of 'a * Lazy<Stream<'a>>

 **~** is a lazy evaluation operator
 this project has StrictData extension enabled, so all fields are strict by default
 so we need to use **~** to make the second field lazy
 it is the oposite of default **!** bang opertor to make something strict
-}

data Stream a = Cons a ~(Stream a)

{-
    below is similr to F# SRTP member function or an interface
-}
instance (Show a) => Show (Stream a) where
  show s = "Stream [" ++ show (streamTakeN 10 s) ++ "...]"

-- instance Num (Stream Integer) where
--     fromInteger n = streamRepeat n
--     negate (Cons x xs) = Cons (negate x) (negate xs)
--     (+) (Cons x xs) (Cons y ys) = Cons (x + y) (xs + ys)
--     (*) (Cons x xs) (Cons y ys) =
--         Cons (x * y) (fromInteger x * ys + xs * fromInteger y + Cons 0 (xs * ys))
--     abs (Cons x xs) = Cons (abs x) (abs xs)
--     signum (Cons x xs) = Cons (signum x) (signum xs)

streamToList :: Stream a -> [a]
streamToList (Cons x xs) = x : streamToList xs

streamTakeN :: Int -> Stream a -> [a]
streamTakeN 0 _ = []
streamTakeN n (Cons x xs) = x : streamTakeN (n - 1) xs

streamRepeat :: a -> Stream a
streamRepeat x = Cons x (streamRepeat x)

streamMap :: (a -> b) -> Stream a -> Stream b
streamMap f (Cons x xs) = Cons (f x) (streamMap f xs)
streamFromSeed :: (Show a) => (a -> a) -> a -> Stream a
streamFromSeed f x =
  Cons x (streamFromSeed f (f x))

-- infinite stream of natural numbers
nats :: Stream Integer
nats = streamFromSeed (+ 1) 0

-- infinite stream of ruller numbers
-- ruller :: Stream Integer
-- ruller =
--     streamMap (largestPowerOf2Dividing . fst) $
--         streamFromSeed (\(n, _) -> (n + 1, 0 :: Integer)) (1, 0)
--   where
--     largestPowerOf2Dividing :: Integer -> Integer
--     largestPowerOf2Dividing n
--         | odd n = 0
--         | otherwise = 1 + largestPowerOf2Dividing (n `div` 2)

-- fibs :: Stream Integer
-- fibs = nats -- TODO
