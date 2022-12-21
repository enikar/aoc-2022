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

-- Hierarchy is the type which represents the file system
type Hierarchy = Map Directory Size

-- One command is a list of String.
-- In case of ls, the first string is "ls", the rest is the
-- listing of the current directory.
-- In case of cd, there is just one String: "cd <dir>" where
-- <dir> is either ".." or a directory present in the current
-- directory
type Cmd = [String]

-- CurrentState is a pair of current directory and the file system.
type CurrentState = (Directory, Hierarchy)

-- XXX I should use a list for the current directory.
-- It'll be more handy to change directory.
-- And I could use also this list as a key for the Map.
-- it'll be less efficient but is it significant?

main :: IO ()
main = do
  datas <- readDatas "input.txt"
  let rootFs = singleton "/" 0
      fs = exeCmds ("/", rootFs) datas
  showSolution "Part1" (part1 fs)
  showSolution "Part2" (part2 fs)

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


-- Add directories found in listing to the Hierarchy.
-- Adjust the size of the current directory by accumulating
-- size of simple file of the listing.
cmdLs :: CurrentState -> [String] -> CurrentState
cmdLs (dir, fs0) listing = (dir, foldl' go fs0 listing)
  where
    readInt s = fromMaybe (errReadInt s) (readMaybe s)
    errReadInt s = error ("CmdLs: Can't parse an Int: " <> s)

    insertDir fs new = insert (intercalate "/" [dir, new]) 0 fs

    incSize fs size = insert dir ((fs ! dir) + size) fs

    go fs s = let [dn, name] = words s
              in case dn of
                   "dir" -> insertDir fs name
                   _     -> incSize fs (readInt dn)

-- execute a cd.
-- When going to the parent. Add the size of the current dir to
-- its parent. Else, just change the current directory. This
-- new current directory has been created in cmdLs.
cmdCd :: String -> CurrentState -> CurrentState
cmdCd new (dir0, fs0) =
  case new of
    ".." -> let dir = intercalate "/" (init (splitOn "/" dir0))
                size0 = fs0 ! dir0
                size = fs0 ! dir
            in
              (dir, insert dir (size+size0) fs0)
    _    -> (intercalate "/" [dir0, new], fs0)

-- roll up to "/" and accumulate size of each traversed directory
-- to its parent.
reportSize :: CurrentState -> Hierarchy
reportSize = snd . until satisfy improve
  where
    satisfy (dir, _) = dir == "/"
    improve  = cmdCd ".."

-- build the file system into a Hierarchy
exeCmds :: CurrentState -> [Cmd] -> Hierarchy
exeCmds (dir0, fs0) = reportSize . foldl' exeOneCmd (dir0, fs0)
  where
    exeOneCmd (dir, fs) cmd =
      let ([c], ls) = splitAt 1 cmd
      in case c of
              "ls" -> cmdLs (dir, fs) ls
              _    -> let [_, dn] = words c -- this is a cd
                      in  cmdCd dn (dir, fs)

readDatas :: String -> IO [Cmd]
readDatas fname = do
  datas <- drop 2 . splitOn "$ " <$> readFile fname
  pure (lines <$> datas)

showSolution :: Show a => String -> a -> IO ()
showSolution part sol =
  putStrLn (part <> ": " <> show sol)
