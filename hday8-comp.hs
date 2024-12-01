-- 2022/12/22
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

-- Parse an Int. Show an error message if it doesn't parse.
readInt :: String -> Int
readInt x = fromMaybe errReadInt (readMaybe x)
  where
    errReadInt = error ("Error: readInt: not an Int: " ++ x)

-- Type synonym for ease declarations.
type Idx = ((Int,Int), (Int,Int))
type MyArray = UArray (Int, Int) Int

-- convert the content to a 2D UArray.
-- Lines are rows of the UArray
formatDatas :: String -> MyArray
formatDatas content = runSTUArray $ do
  a <- newArray_ ((1,1), (m, n))
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
  arr <-  formatDatas <$> readFile "day8.txt"
  printSolution "Part1" (part1 arr)
  printSolution "Part2" (part2 arr)

printSolution :: Show a => String -> a -> IO ()
printSolution part sol =
  putStrLn (part ++ ": " ++ show sol)

part1 :: MyArray -> Int
part1 = foldl' countVisible 0 . compute visible
  where
    countVisible acc True = acc + 1
    countVisible acc _    = acc

part2 :: MyArray -> Int
part2 = maximum . compute scenicScore

{-# INLINE compute #-}
-- compute a list from the UArray, applying a function =f= for
-- each element of the UArray.
compute :: (Idx -> MyArray -> (Int, Int) -> a) -> MyArray -> [a]
compute f arr = [f ixes arr (i,j)| j <- [yi..ys], i <- [xi..xs]]
  where ixes@((xi, yi), (xs, ys)) = bounds arr

-- Is the tree at (x0, y0) visible from the edge ?
visible :: Idx -> MyArray -> (Int, Int) -> Bool
visible ((xi, yi), (xs, ys))  arr (x0,y0) =
  x0 == xi || x0 == xs || y0 == yi || y0 == ys ||
  all infTree xInf ||
  all infTree xSup ||
  all infTree yInf ||
  all infTree ySup
  where
    tree = arr ! (x0,y0)
    (xInf, xSup, yInf, ySup) = ranges arr (x0,y0)

    -- a tree is visible if all trees from the edge to the tree is
    -- smaller than it
    infTree ix = arr ! ix < tree

-- Calculate the scenic score of tree at (x0, y0).
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
    (xInf, xSup, yInf, ySup) = ranges arr (x0,y0)

    -- Count trees from (x0,y0) to edges while they are smaller than
    -- this at (x0, y0)
    trees = fst . foldl' untilBlocked (0, False)
      where
        untilBlocked (n, True)  _ = (n, True)
        untilBlocked (n, False) ix
          | tree > arr ! ix  = (n+1, False)
          | tree <= arr ! ix = (n+1, True)
          | otherwise        = (n, True) -- Not reach

{-# INLINE ranges #-}
-- Return a tuple of needed ranges to explore the neighbourhood
-- at (x0, y0)
ranges :: MyArray
          -> (Int, Int)
          -> ([(Int, Int)], [(Int, Int)], [(Int, Int)], [(Int, Int)])
ranges arr (x0, y0) = (xInf, xSup, yInf, ySup)
  where
    -- indices of intervals in the columns x0 and the row y0
    -- before and after the point at (x0, y0)

    -- xInf = [(x,y) | (x,y) <- allIx, x == x0, y < y0]
    -- xSup = [(x,y) | (x,y) <- allIx, x == x0, y > y0]
    -- yInf = [(x,y) | (x,y) <- allIx, y == y0, x < x0]
    -- ySup = [(x,y) | (x,y) <- allIx, y == y0, x > x0]
    -- Alternative definition. It should be faster.
    allIx = indices arr
    xIx = filter (\(x, _) -> x == x0) allIx
    yIx = filter (\(_, y) -> y == y0) allIx
    [xInf, _, xSup] = groupSortOn (\(_, y) -> compare y y0) xIx
    [yInf, _, ySup] = groupSortOn (\(x, _) -> compare x x0) yIx
