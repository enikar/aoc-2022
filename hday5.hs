-- 2022/12/05
-- Solution of AoC 2022 5th day.

import Data.List.Extra (splitOn)
import Data.List (transpose)
import Control.Monad (void)
import Data.Char (isDigit)
import qualified Data.IntMap.Strict as M
import Data.IntMap.Strict (IntMap)
import Data.Maybe (fromMaybe)
import Text.ParserCombinators.ReadP (ReadP
                                    ,skipSpaces
                                    ,readP_to_S
                                    ,eof
                                    ,munch1
                                    ,string
                                    )

data Move = Move {number :: Int
                 ,from :: Int
                 ,dest :: Int} deriving (Show)

type Stacks = IntMap [String]

type Datas = (Stacks, [Move])

main :: IO ()
main = do
  datas <- formatDatas <$> readFile "day5.txt"
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
    ins = splitOn "\n\n" input
    q = case ins of
      [x,_] -> x
      _     -> error "formatDatas: malformed input."
    m = case ins of
      [_,x] -> x
      _     -> error "formatDatas: malformed input."

makeStacks :: String -> Stacks
makeStacks q = snd (foldl' buildStack e0 ls)
  where
    qs = init (lines q)
    ls = transpose (map readStacks qs)
    e0 = (1, M.empty)
    trim = dropWhile ("" ==)
    buildStack (cnt, stacks0) x = (cnt+1, stacks)
      where
        stacks = M.insert cnt (trim x) stacks0

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
  where
    f ls = case readP_to_S readMove ls of
             (x:_) -> fst x
             []    -> error "buildMoves: a line is empty."

readMove :: ReadP Move
readMove = do
  void (string "move")
  skipSpaces
  n <- integer
  skipSpaces
  void (string "from")
  skipSpaces
  f <- integer
  skipSpaces
  void (string "to")
  skipSpaces
  t <- integer
  eof
  pure (Move n f t)

integer :: ReadP Int
integer = read <$> munch1 isDigit
