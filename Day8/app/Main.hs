module Main where

import Data.List (transpose)

solve :: [[Int]] -> (Int, Int)
solve grid = (solve' checkLine sum, solve' checkScore maximum)
  where
    solve' f1 f2 = f2 [f1 (x, y) grid | x <- [0 .. 98], y <- [0 .. 98]]

getLines :: (Int, Int) -> [[Int]] -> ([Int], [Int], [Int], [Int], Int)
getLines (x, y) grid = (up, drop 1 down, left, drop 1 right, grid !! y !! x)
  where
    (left, right) = splitAt x (grid !! y)
    (up, down) = splitAt y (map (!! x) grid)

checkLine :: (Int, Int) -> [[Int]] -> Int
checkLine (x, y) grid = if any (all (< m)) [left, right, up, down] then 1 else 0
  where
    (up, down, left, right, m) = getLines (x, y) grid

checkScore :: (Int, Int) -> [[Int]] -> Int
checkScore (x, y) grid = product $ map (f 0) [reverse left, right, reverse up, down]
  where
    f :: Int -> [Int] -> Int
    f n [] = n
    f n (z : zs) = if z < m then f (n + 1) zs else n + 1
    (up, down, left, right, m) = getLines (x, y) grid

main :: IO ()
main = do
  myFile <- readFile "input.txt"
  let trees = (map (map (read . (: []))) . lines) myFile
  print $ solve trees