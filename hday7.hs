-- 2022/12/07
-- Solution of AoC 2022 7th day.

import Data.List (intercalate, foldl')
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

cmdCd :: String -> CurrentState -> CurrentState
cmdCd new (dir0, fs0) =
  case new of
    ".." -> let dir = intercalate "/" (init (splitOn "/" dir0))
                size0 = fs0 ! dir0
                size = fs0 ! dir
            in
              (dir, insert dir (size+size0) fs0)
    _    -> (intercalate "/" [dir0, new], fs0)


reportSize :: CurrentState -> Hierarchy
reportSize = snd . until satisfy improve
  where
    satisfy (dir, _) = dir == "/"
    improve  = cmdCd ".."

exeCmds :: CurrentState -> [Cmd] -> Hierarchy
exeCmds (dir0, fs0) = reportSize . foldl' go (dir0, fs0)
  where
    go (dir, fs) cmd =
      let ([c], ls) = splitAt 1 cmd
      in case c of
              "ls" -> let (size, dirs) = cmdLs dir ls
                          fs' = foldl' createDirs (insert dir size fs) dirs
                          createDirs acc d = insert d 0 acc
                      in (dir, fs')
              _    -> let [_, dn] = words c -- this is a cd
                      in  cmdCd dn (dir, fs)


part1 :: Hierarchy -> Int
part1 = M.foldl' sumIt 0
  where sumIt acc v
          | v <= 100000 = v + acc
          | otherwise   = acc

part2 :: Hierarchy -> Int
part2 fs = M.foldl' minimize rootSize fs
  where
    rootSize = 70000000
    occupied = fs ! "/"
    freeSpace = rootSize - occupied
    needed = 30000000 - freeSpace

    minimize acc v
      |v >= needed = min acc v
      |otherwise   = acc
