module Basics (
  mapX,
  foldrX,
  filterX,
  concatMapX,
  quickSort,
) where

mapX :: (a -> b) -> [a] -> [b]
mapX _ [] = []
mapX f (item : t) =
  f item : mapX f t
foldrX :: (a -> acc -> acc) -> acc -> [a] -> acc
foldrX _ acc [] = acc
foldrX f acc (i : t) =
  foldrX f (f i acc) t

filterX :: (a -> Maybe a) -> [a] -> [a]
filterX _ [] = []
filterX f (i : t) =
  case f i of
    Just s -> s : filterX f t
    Nothing -> filterX f t
concatMapX :: (a -> [b]) -> [a] -> [b]
concatMapX _ [] = []
concatMapX f (i : t) =
  f i ++ concatMapX f t

quickSort :: (Ord a) => [a] -> [a]
quickSort [] = []
quickSort [x] = [x]
quickSort (x : xs) =
  let
    (lt, eq, gt) = foldr st ([], [x], []) xs
    st a (l, e, g)
      | a < x = (a : l, e, g)
      | a == x = (l, a : e, g)
      | otherwise = (l, e, a : g)
   in
    quickSort lt ++ eq ++ quickSort gt
