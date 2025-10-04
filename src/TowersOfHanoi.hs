module TowersOfHanoi (Peg, Move, hanoi) where

-- Exercise 5 of https://www.seas.upenn.edu/~cis1940/spring13/hw/01-intro.pdf
type Peg = String
type Move = (Peg, Peg)
hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi n src dst aux =
  case n of
    0 -> []
    1 -> [(src, dst)]
    _ ->
      hanoi (n - 1) src aux dst -- move n-1 disks from src to aux
        <> [(src, dst)] -- move the largest disk from src to dst
        <> hanoi (n - 1) aux dst src -- move n-1 disks from aux to dst
