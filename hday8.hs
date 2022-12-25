-- 2022/12/02
-- Solution of AoC 2022, 8th day

import Data.List.Extra (chunksOf, groupSortOn)
import Data.Foldable (foldl', forM_)
import Data.Array.Unboxed (UArray
                          ,elems
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


formatDatas :: String -> [[Int]]
formatDatas content =
  fmap readInt . chunksOf 1 <$> lines content


main :: IO ()
main = do
  arr <- fromLists . formatDatas <$> readFile "input.txt"
  printSolution "Part1" (part1 arr)
  printSolution "Part2" (part2 arr)

printSolution :: Show a => String -> a -> IO ()
printSolution part sol =
  putStrLn (part <> ": " <> show sol)

part1 :: UArray (Int, Int) Int -> Int
part1 = foldl' countVisible 0 . elems . visibilities
  where
    countVisible acc True = acc + 1
    countVisible acc _    = acc

part2 :: UArray (Int, Int) Int -> Int
part2 = maximum . elems . scenicScores

type Idx = ((Int,Int), (Int,Int))

indexes :: Int -> Int -> Idx
indexes m n = ((1,1), (m,n))

fromLists :: [[Int]] -> UArray (Int, Int) Int
fromLists datas = runSTUArray $ do
   a <- newArray_ (indexes m n)
   forM_ (zip [1..n] datas) $ \(j, xs) ->
     forM_ (zip [1..m] xs) $ \(i, x) ->
       writeArray a (i, j) x
   pure a
     where
       m = length (head datas)
       n = length datas

visibilities :: UArray (Int, Int) Int -> UArray (Int, Int) Bool
visibilities arr = runSTUArray $ do
  let ixes@((xi, yi), (xs, ys)) = bounds arr
  a <- newArray_ ixes
  forM_ [yi..ys] $ \j ->
    forM_ [xi..xs] $ \i ->
      writeArray a (i,j) (isVisible ixes arr (i,j))
  pure a

isVisible :: Idx -> UArray (Int,Int) Int -> (Int, Int) -> Bool
isVisible ((xi, yi), (xs, ys))  arr (x0,y0) =
  x0 == xi || x0 == xs || y0 == yi || y0 == ys ||
  all (\ix -> arr ! ix < tree) xInf ||
  all (\ix -> arr ! ix < tree) xSup ||
  all (\ix -> arr ! ix < tree) yInf ||
  all (\ix -> arr ! ix < tree) ySup
  where
    tree = arr ! (x0,y0)
    allIdx = indices arr
    xIdx = filter (\(x, _) -> x == x0) allIdx
    yIdx = filter (\(_, y) -> y == y0) allIdx
    [xInf, _, xSup] = groupSortOn (\(_, y) -> compare y y0) xIdx
    [yInf, _, ySup] = groupSortOn (\(x, _) -> compare x x0) yIdx

scenicScores :: UArray (Int, Int) Int -> UArray (Int, Int) Int
scenicScores arr = runSTUArray $ do
  let ixes@((xi, yi), (xs, ys)) = bounds arr
  a <- newArray_ ixes
  forM_ [yi..ys] $ \j ->
    forM_ [xi..xs] $ \i ->
      writeArray a (i,j) (scenicScore ixes arr (i,j))
  pure a


scenicScore :: Idx -> UArray (Int, Int) Int -> (Int, Int) -> Int
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
