-- 2022/12/02
-- Solution of AoC 2022, 8th day

import Data.List.Extra (chunksOf, groupSortOn)
import Data.Foldable (foldl', forM_)

import Data.Array.Unboxed (UArray
                          ,bounds
                          ,indices
                          ,(!))

import Data.Array.ST (runSTUArray
                     ,newArray_
                     ,writeArray)

import Data.Maybe (fromMaybe)
import Text.Read (readMaybe)

readInt :: String -> Int
readInt x = fromMaybe errReadInt (readMaybe x)
  where
    errReadInt = error ("Error: readInt: not an Int: " <> x)

type Idx = ((Int,Int), (Int,Int))
type MyArray = UArray (Int, Int) Int

indexes :: Int -> Int -> Idx
indexes m n = ((1,1), (m,n))


formatDatas :: String -> MyArray
formatDatas content = runSTUArray $ do
  a <- newArray_ (indexes m n)
  forM_ (zip [1..] ls) $ \(j, line) ->
    forM_ (zip [1..] (chunksOf 1 line)) $ \(i, s) ->
      writeArray a (i, j) (readInt s)
  pure a
      where
        ls = lines content
        n = length ls
        m = length (head ls)

main :: IO ()
main = do
  arr <-  formatDatas <$> readFile "input.txt"
  printSolution "Part1" (part1 arr)
  printSolution "Part2" (part2 arr)

printSolution :: Show a => String -> a -> IO ()
printSolution part sol =
  putStrLn (part <> ": " <> show sol)

part1 :: MyArray -> Int
part1 = foldl' countVisible 0 . compute isVisible
  where
    countVisible acc True = acc + 1
    countVisible acc _    = acc

part2 :: MyArray -> Int
part2 = maximum . compute scenicScore

{-# INLINE compute #-}
compute :: (Idx -> MyArray -> (Int, Int) -> a) -> MyArray -> [a]
compute f arr =
  [f ixes arr (i,j)| j <- [yi..ys], i <- [xi..xs]]

  where ixes@((xi, yi), (xs, ys)) = bounds arr

isVisible :: Idx -> MyArray -> (Int, Int) -> Bool
isVisible ((xi, yi), (xs, ys))  arr (x0,y0) =
  x0 == xi || x0 == xs || y0 == yi || y0 == ys ||
  all infTree xInf ||
  all infTree xSup ||
  all infTree yInf ||
  all infTree ySup
  where
    tree = arr ! (x0,y0)
    allIdx = indices arr
    xIdx = filter (\(x, _) -> x == x0) allIdx
    yIdx = filter (\(_, y) -> y == y0) allIdx
    [xInf, _, xSup] = groupSortOn (\(_, y) -> compare y y0) xIdx
    [yInf, _, ySup] = groupSortOn (\(x, _) -> compare x x0) yIdx

    infTree ix = arr ! ix < tree

scenicScore :: Idx -> MyArray -> (Int, Int) -> Int
scenicScore ((xi,yi),(xs, ys)) arr (x0,y0) =
  if x0 == xi || x0 == xs || y0 == yi || y0 == ys
  then 0
  else trees (reverse xInf)
       * trees xSup
       * trees (reverse yInf)
       * trees ySup
  where
    tree = arr ! (x0, y0)
    allIdx = indices arr
    xIdx = filter (\(x, _) -> x == x0) allIdx
    yIdx = filter (\(_, y) -> y == y0) allIdx
    [xInf, _, xSup] = groupSortOn (\(_, y) -> compare y y0) xIdx
    [yInf, _, ySup] = groupSortOn (\(x, _) -> compare x x0) yIdx

    trees = fst . foldl' untilBlocked (0, False)
      where
        untilBlocked (n, True)  _ = (n, True)
        untilBlocked (n, False) ix
          | tree > arr ! ix  = (n+1, False)
          | tree <= arr ! ix = (n+1, True)
          | otherwise        = (n, True) -- Not reach
