-- 2022/12/05
-- Solution of AoC 2022 5th day.

import Data.List.Extra (splitOn)
import Data.List (transpose)
import Text.ParserCombinators.ReadP
import Control.Applicative ((<|>))
import Control.Monad (void, unless)
import Control.Monad.Loops (allM)
import Data.Char (isDigit)
import qualified Data.IntMap.Strict as M
import Data.IntMap.Strict (IntMap)
import Data.Foldable (foldl')
import Data.Maybe (fromMaybe)

data Move = Move {number :: Int
                 ,from :: Int
                 ,dest :: Int} deriving (Show)

type Stacks = IntMap [String]

type Datas = (Stacks, [Move])

main :: IO ()
main = do
  datas <- formatDatas <$> readFile "input.txt"
  showSolution "Part1" (part1 datas)
  showSolution "Part2" (part2 datas)


part1 :: Datas -> String
part1 (stacks, moves) = getTops (foldl' (moveCrates reverse) stacks moves)

part2 :: Datas -> String
part2 (stacks, moves) = getTops (foldl' (moveCrates id) stacks moves)

getTops :: Stacks -> String
getTops = M.foldl' collectTop ""
  where
    collectTop acc (x:_) = acc ++ x
    collectTop _ _       = errGetTop

    errGetTop = error "getTops: stack underflow."


moveCrates :: ([String] -> [String]) -> Stacks -> Move -> Stacks
moveCrates f sts0 mv = M.adjust (const (f stt ++ stack1)) to sts1
     where
       errLookup k = error ("Part1: bad key: " <> show k)
       n  = number mv
       fr = from mv
       to = dest mv
       stack0 = fromMaybe (errLookup fr) (M.lookup fr sts0)
       (stt, stf) = splitAt n stack0
       sts1 = M.adjust (const stf) fr sts0
       stack1 = fromMaybe (errLookup to) (M.lookup to sts1)


showSolution :: Show a => String -> a -> IO ()
showSolution part sol =
  putStrLn (part <> ": " <> show sol)


formatDatas :: String -> (Stacks, [Move])
formatDatas input = (makeStacks q, buildMoves m)
  where
    [q, m] = splitOn "\n\n" input

makeStacks :: String -> Stacks
makeStacks q = snd (foldl' buildStack e0 ls)
  where
    qs = tail (reverse (lines q))
    ls = transpose (map readStacks qs)
    e0 = (1, M.empty)
    trim = takeWhile ("" /=)
    buildStack (cnt, stacks0) x = (cnt+1, stacks)
      where
        stacks = M.insert cnt (reverse (trim x)) stacks0

readStacks :: String -> [String]
readStacks "" = []
readStacks s  = readCrate h : readStacks rest
  where
    (h, t) = splitAt 3 s
    rest = case t of
             (' ':r) -> r  -- skip one space
             ""      -> "" -- but not at the end
             _       -> error ("readStacks: bad format: " <> t)

readCrate :: String -> String
readCrate cs = case cs of
                 "   "         -> ""
                 ['[', c, ']'] -> [c]
                 _ -> error ("readCrate: bad crate format:" <> cs)

buildMoves :: String -> [Move]
buildMoves = map f . lines
  where f = fst . head . readP_to_S readMove

readMove :: ReadP Move
readMove = do
  skipString "move"
  skipSpaces
  n <- numbers 1 <|> numbers 2
  skipSpaces
  skipString "from"
  skipSpaces
  f <- numbers 1
  skipSpaces
  skipString "to"
  skipSpaces
  t <- numbers 1
  eof
  pure (Move n f t)

digit :: ReadP Char
digit = satisfy isDigit

numbers :: Int -> ReadP Int
numbers n = fmap read (count n digit)

skipString :: String -> ReadP ()
skipString this = do
  -- mequal :: Ord a => (a, a) -> ReadP Bool
  -- mequal consumes input if x == y
  let mequal (x,y) = if x == y
                     then do void get
                             pure True
                     else pure False
  s <- look
  p <- allM mequal (zip s this)
  unless p pfail
