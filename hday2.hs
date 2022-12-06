-- 2022/12/02
-- Solution of Day2 of AoC 2022

-- import Data.List.Extra (splitOn)
-- import Data.List (sortBy)
import Data.Foldable (foldl')
-- import Data.Maybe (fromMaybe)
-- import Text.Read (readMaybe)

-- data Choice = Rock | Papper | Scissors
-- data Game = Defeat | Draw | Win

main :: IO ()
main = do
  datas <- lines <$> readFile "input.txt"
  showSolution "Part1" (solution pointsPart1 datas)
  showSolution "Part2" (solution pointsPart2 datas)

solution :: ([String] -> Int) -> [String] -> Int
solution points = foldl' sumIt 0
  where
    sumIt acc game = acc + points (words game)


errPoints :: [String] -> a
errPoints s = error ("Invalid input: " <> show s)

second :: [a] -> a
second = head . tail

-- For the game A = Rock, B = Paper and C = Scissors.
-- For part1: X = Rock, Y = Paper and Z = Scissors
pointsPart1 :: [String] -> Int
pointsPart1 game =
  case head game of
    "A" -> case second game of
             "X" -> 4 -- 1 + 3
             "Y" -> 8 -- 2 + 6
             "Z" -> 3 -- 3 + 0
             _ -> errPoints game
    "B" -> case second game of
             "X" -> 1 -- 1 + 0
             "Y" -> 5 -- 2 + 3
             "Z" -> 9 -- 3 + 6
             _ -> errPoints game
    "C" -> case second game of
             "X" -> 7 -- 1 + 6
             "Y" -> 2 -- 2 + 0
             "Z" -> 6 -- 3 + 3
             _ -> errPoints game
    _  -> errPoints game

-- For the game A = Rock = 1, B = Paper = 2 and C = Scissors = 3
-- For part2: X = defeated = 0, Y = draw = 3, Z = win = 6
pointsPart2 :: [String] -> Int
pointsPart2 game =
  case head game of
    "A" -> case second game of -- Rock
             "X" ->  3  -- defeat = Scissor = 3 + 0
             "Y" ->  4  -- draw = Rock  = 1 + 3
             "Z" ->  8  -- win = Paper = 2 + 6
             _  -> errPoints game
    "B" -> case second game of -- Papper
             "X" -> 1  -- defeat = Rock = 1 + 0
             "Y" -> 5  -- draw = Papper = 2 + 3
             "Z" -> 9  -- Win = Scissor = 3 + 6
             _ -> errPoints game
    "C" -> case second game of -- Scissor
             "X" -> 2  -- defeat = Papper = 2 + 0
             "Y" -> 6  -- draw = Scissor = 3 + 3
             "Z" -> 7  -- win = Rock = 1 + 6
             _ -> errPoints game
    _ -> errPoints game


showSolution :: String -> Int -> IO ()
showSolution part sol =
  putStrLn (part <> ": " <> show sol)
