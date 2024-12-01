-- 2022/12/06
-- Solution of AoC 2022, 6th day

import Data.List (tails)
import Data.Foldable (foldl')
import qualified Data.Set as S

main :: IO ()
main = do
  datas <- readFile "day6.txt"
  showSolution "Part1" (part1 datas)
  showSolution "Part2" (part2 datas)

part1 :: String -> Int
part1 = getPacketMarker 4

part2 :: String -> Int
part2 = getPacketMarker 14

getPacketMarker :: Int -> String -> Int
getPacketMarker n = fst . foldl' f (n-1, False) . tails
  where
    f (cnt, b) s
      | b         = (cnt, b)
      | p         = (cnt+1, True)
      | otherwise = (cnt+1, False)
        where
          p = S.size (S.fromList (take n s)) == n


showSolution :: Show a => String -> a -> IO ()
showSolution part sol =
  putStrLn (part <> ": " <> show sol)
