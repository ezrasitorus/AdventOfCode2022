module Main where

import Data.List.Split (splitOn)

data Directory = Directory [Directory] Int deriving (Show, Eq)

solve :: [String] -> Directory -> (Directory, [String])
solve [] d = (d, [])
solve ("$ cd .." : ss) d = (d, ss)
solve (('$' : _) : ss) (Directory ds is) =
  solve ss' (Directory (d : ds) is)
  where
    (d, ss') = solve ss (Directory [] 0)
solve (s : ss) (Directory ds is) = solve ss (Directory ds (i + is))
  where
    i = (read . head . splitOn " ") s :: Int

getSize :: Directory -> Int
getSize (Directory [] i) = i
getSize (Directory ds i) = sum $ i : map getSize ds

answer :: Directory -> Int
answer (Directory [] is) = if is > 100000 then 0 else is
answer d@(Directory ds is) = sum $ (if getSize d > 100000 then 0 else getSize d) : map answer ds

answer2 :: Directory -> [Int]
answer2 (Directory [] is) = [is]
answer2 (Directory ds is) = sum (is : map getSize ds) : concatMap answer2 ds

main :: IO ()
main = do
  myFile <- readFile "input.txt"
  let input = (tail . filter (\x -> take 3 x /= "dir" && take 4 x /= "$ ls") . lines) myFile
  let dir = fst . solve input $ Directory [] 0
  print (answer dir, (minimum . filter (>= getSize dir - 40000000) . answer2) dir)
