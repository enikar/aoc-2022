-- 2022/12/03
-- Solution of Day3 of AoC 2022

import Data.Foldable (foldl')
import Data.Char (ord)
import Data.List (intersect, nub)

main :: IO ()
main = do
  datas <- lines <$> readFile "day3.txt"
  showSolution "Part1" (part1 datas)
  showSolution "Part2" (part2 datas)

priority :: Char -> Int
priority c
  | ord 'a' <= o && o <= ord 'z' = 1 + o - ord 'a'
  | ord 'A' <= o && o <= ord 'Z' = 27 + o - ord 'A'
  | otherwise = error ("Priority: bad input: " <> show c)
  where
    o = ord c

getDuplicate :: String -> Char
getDuplicate bag =
  let size = length bag
      cSize = size `div` 2
      c1 = take cSize bag
      c2 = drop cSize bag
      inter = nub (c1 `intersect` c2)
  in
    if even size
    then case inter of
           [c] -> c
           _   -> error ("getDuplicate: don't intersect to one element: " <> bag)
    else
      error ("getDuplicate: number of elements is not a even: " <> show size)


part1 :: [String] -> Int
part1 = foldl' sumIt 0
  where
    sumIt acc bag = acc + priority (getDuplicate bag)

-- groupBagsTCO :: [String] -> [[String]]
-- groupBagsTCO = go []
--   where
--     go ls [] = ls
--     go ls rest =
--       let (h, t) = splitAt 3 rest
--       in go (h:ls) t

groupBagsR :: [String] -> [[String]]
groupBagsR [] = []
groupBagsR rest = h : groupBagsR t
  where (h, t) = splitAt 3 rest

getBadge :: [String] -> Char
getBadge  [b1, b2, b3] =
  let bs = nub (b1 `intersect` b2 `intersect` b3)
  in case bs of
       [c] -> c
       _   -> error ("getBadge: expected singleton intersect: " <> show [b1, b2, b3])
getBadge _ =  error "Expected a list of 3 elements."

part2 :: [String] -> Int
part2 = foldl' sumIt 0 . groupBagsR
  where
    sumIt acc g = acc + priority (getBadge g)


showSolution :: String -> Int -> IO ()
showSolution part sol =
  putStrLn (part <> ": " <> show sol)
