-- 2022/12/05
-- Solution of Aoc 2022, 4th day.

import Data.List.Extra (splitOn)
import Data.Foldable (foldl')
import Data.Maybe (fromMaybe)
import Text.Read (readMaybe)
import Data.Set (Set, fromList, isSubsetOf, disjoint)

type Datas = [(Set Int, Set Int)]

formatDatas :: String -> Datas
formatDatas = map buildSetPairs . lines
  where
    buildSetPairs s = case map toRange (splitOn "," s) of
                        [s1, s2] -> (s1, s2)
                        _        -> error ("formatDatas: bad argument" <> s)
      where
        toRange x = fromList [n1..n2]
          where
            (n1, n2) = convertInts (splitOn "-" x)

            convertInts [x1, x2] = (readInt x1, readInt x2)
            convertInts _ = error ("formatDatas: bad range argument: " <> x)

            readInt = fromMaybe errReadInt . readMaybe
            errReadInt = error "Error: formatDatas: not an Int"


main :: IO ()
main = do
  datas <- formatDatas <$> readFile "input.txt"
  showSolution "Part1" (part1 datas)
  showSolution "Part2" (part2 datas)

part1 :: Datas -> Int
part1 = foldl' countIncluded 0
  where
    countIncluded acc x
      | included x = acc + 1
      | otherwise  = acc

    included (s1, s2) = s1 `isSubsetOf` s2 || s2 `isSubsetOf` s1

part2 :: Datas -> Int
part2 = foldl' countOverlaped 0
  where
    countOverlaped acc x
      | overlapped x = acc + 1
      | otherwise    = acc

    overlapped (s1, s2) = not (s1 `disjoint` s2)

showSolution :: String -> Int -> IO ()
showSolution part sol =
  putStrLn (part <> ": " <> show sol)
