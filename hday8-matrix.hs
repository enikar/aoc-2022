-- 2022/12/29
-- Solution of AoC 2022, 8th day

import Data.Foldable (foldl')

import Numeric.LinearAlgebra.Data

import Data.Maybe (fromMaybe)
import Text.Read (readMaybe)

-- Parse a CInt. Show an error message if it doesn't parse.
readCInt :: String -> I
readCInt s = fromMaybe errMyRead (readMaybe s)
  where
    errMyRead = error ("Error: readCInt: not a CInt: " <> s)

type MyMatrix = Matrix I

-- Convert the string =content= to a (Matrix I)
formatDatas :: String -> MyMatrix
formatDatas content = fromLists lss
  where
    lss = fmap readCInt . map (: []) <$> lines content

main :: IO ()
main = do
  arr <-  formatDatas <$> readFile "input.txt"
  printSolution "Part1" (part1 arr)
  printSolution "Part2" (part2 arr)

printSolution :: Show a => String -> a -> IO ()
printSolution part sol =
  putStrLn (part <> ": " <> show sol)

part1 :: MyMatrix -> Int
part1 = foldl' countVisible 0 . compute visible
  where
    countVisible acc True = acc + 1
    countVisible acc _    = acc

part2 :: MyMatrix -> Int
part2 = maximum . compute scenicScore

{-# INLINE compute #-}
-- compute a list from the Matrix =mat=, applying a function =f= for
-- each element of the Matrix
compute :: ((Int,Int) -> MyMatrix -> (Int, Int) -> a) -> MyMatrix -> [a]
compute f mat = [f (cMax,rMax) mat (i,j)| j <- [0..rMax], i <- [0..cMax]]
  where
    (r, c) = size mat
    cMax = c-1
    rMax = r-1

-- Is the tree at (x0, y0) visible from the edge ?
visible :: (Int,Int) -> MyMatrix -> (Int, Int) -> Bool
visible (xs,ys) mat (x0,y0) =
  x0 == 0 || x0 == xs || y0 == 0 || y0 == ys ||
  all infTree xInf ||
  all infTree xSup ||
  all infTree yInf ||
  all infTree ySup
  where
    tree = mat `atIndex` (y0,x0)
    (xInf, xSup, yInf, ySup) = neighbour mat (x0,y0)

    -- a tree is visible if all trees from the edge to the tree is
    -- smaller than it
    infTree t = t < tree

-- Calculate the scenic score of tree at (x0, y0).
scenicScore :: (Int,Int) -> MyMatrix -> (Int, Int) -> Int
scenicScore (xs, ys) mat (x0,y0) =
  if x0 == 0 || x0 == xs || y0 == 0 || y0 == ys
  then 0
  else trees (reverse xInf)
       * trees xSup
       * trees (reverse yInf)
       * trees ySup
  where
    tree = mat `atIndex` (y0, x0)
    (xInf, xSup, yInf, ySup) = neighbour mat (x0,y0)

    trees = fst . foldl' untilBlocked (0, False)
      where
        untilBlocked (n, True)  _ = (n, True)
        untilBlocked (n, False) t
          | tree > t  = (n+1, False)
          | tree <= t = (n+1, True)
          | otherwise = (n, True) -- Not reach


{-# INLINE neighbour #-}
-- Return a tuple of values at the neighbourhood of (x0, y0)
neighbour :: MyMatrix
          -> (Int, Int)
          -> ([I], [I], [I],[I])
neighbour mat (x0, y0) = (xInf, xSup, yInf, ySup)
  where
    -- x and y are reversed for matrix manipulation.
    -- we need to give (row, column) as the index.
    -- y means row, x means column
    xInf = concat . toLists $ mat ?? (Take y0, Pos (idxs [x0]))
    xSup = concat . toLists $ mat ?? (Drop (y0+1), Pos (idxs [x0]))
    yInf = head . toLists $ mat ?? (Pos (idxs [y0]), Take x0)
    ySup = head . toLists $ mat ?? (Pos (idxs [y0]), Drop (x0+1))

-- for testing in ghci
sample :: IO MyMatrix
sample = formatDatas <$> readFile "sample.txt"

view :: MyMatrix -> IO ()
view = disp 0 . cmap fromIntegral
