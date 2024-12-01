-- 2022/12/02
-- Solution of Day1 of AoC 2022

import Data.List.Extra (splitOn)
import Data.List (sortBy)
import Data.Foldable (foldl')
import Data.Maybe (fromMaybe)
import Text.Read (readMaybe)

formatDatas :: String -> [Int]
formatDatas datas = sortBy (flip compare) scoresAsInts
  where
    scoresAsInts = map sumScores allScores

    allScores = splitOn "\n\n" datas

    sumScores = foldl' sumIt 0 . lines

    sumIt acc x = acc + readInt x

    readInt  = fromMaybe errReadInt . readMaybe

    errReadInt = error "Error: formatDatas: not an Int"


main :: IO ()
main = do
  datas <- formatDatas <$> readFile "day1.txt"
  showSolution "Part1" (part1 datas)
  showSolution "Part2" (part2 datas)

showSolution :: String -> Int -> IO ()
showSolution part sol =
  putStrLn (part <> ": " <> show sol)

part1, part2 :: [Int] -> Int
part1 = head
part2 = sum . take 3
