-- 2022/12/07
-- Solution of AoC 2022 7th day.

import Data.List (intercalate, scanl', foldl')
import Data.List.Extra (splitOn)

import Data.Maybe (fromMaybe)
import Text.Read (readMaybe)

import Data.Map.Strict (Map, (!), insert, singleton)
import qualified Data.Map.Strict as M


type Directory = String
type Size = Int
type Hierarchy = Map Directory Size
type Cmd = [String]
type CurrentState = (Directory, Hierarchy)

readDatas :: String -> IO [[String]]
readDatas fname = do
  datas <- drop 2 . splitOn "$ " <$> readFile fname
  pure (lines <$> datas)

main :: IO ()
main = do
  datas <- readDatas "input.txt"
  let rootFs = singleton "/" 0
      fs = exeCmds ("/", rootFs) datas
  showSolution "Part1" (part1 fs)
  showSolution "Part2" (part2 fs)

showSolution :: Show a => String -> a -> IO ()
showSolution part sol =
  putStrLn (part <> ": " <> show sol)

cmdLs :: Directory -> [String] -> (Size, [Directory])
cmdLs dir = foldl' go (0, [])
  where
    readInt s = fromMaybe (errReadInt s) (readMaybe s)
    errReadInt s = error ("CmdLs: Can't parse an Int: " <> s)

    go (size, dirs) x =
      let [dn, name] = words x
      in case dn of
        "dir" -> (size, intercalate "/" [dir , name] : dirs)
        _     -> (size + readInt dn, dirs)

cmdCd :: Directory -> String -> Hierarchy -> (Hierarchy, Directory)
cmdCd dir0 new fs0 =
  case new of
    ".." -> let dir = intercalate "/" (init (splitOn "/" dir0))
                size0 = fs0 ! dir0
                size = fs0 ! dir
            in
              (insert dir (size+size0) fs0, dir)
    _    -> (fs0, intercalate "/" [dir0, new])


buildDirs :: String -> [String]
buildDirs dir = drop 1 (scanl' go "" ls)
  where go acc s = intercalate "/" [acc, s]
        ls = drop 1 (splitOn "/" dir)

collectSize :: (Directory, Hierarchy) -> Hierarchy
collectSize (dir, h0) = fst $ foldr go (h0, size0) dirs
  where
    dirs = init (buildDirs dir)
    size0 = h0 ! dir
    go d (h, size) = let size' =  size + h ! d
                     in (insert d size' h, size')

exeCmds :: CurrentState -> [Cmd] -> Hierarchy
exeCmds (dir0, fs0) = collectSize . foldl' go (dir0, fs0)
  where
    go (dir, fs) cmd =
      let ([c], ls) = splitAt 1 cmd
      in case c of
              "ls" -> let (size, dirs) = cmdLs dir ls
                          fs' = foldl' createDirs (insert dir size fs) dirs
                          createDirs acc d = insert d 0 acc
                      in (dir, fs')
              _    -> let [_, dn] = words c -- this is a cd
                          (fs', new) = cmdCd dir dn fs
                      in (new, fs')


part1 :: Hierarchy -> Int
part1 = M.foldl' sumIt 0
  where sumIt acc v
          | v <= 100000 = v + acc
          | otherwise   = acc

part2 :: Hierarchy -> Int
part2 fs = M.foldl' minimize totalSpace fs
  where
    totalSpace = 70000000
    rootSize = fs ! "/"
    freeSpace = totalSpace - rootSize
    needed = 30000000 - freeSpace

    minimize acc v
      |v >= needed = min acc v
      |otherwise   = acc
