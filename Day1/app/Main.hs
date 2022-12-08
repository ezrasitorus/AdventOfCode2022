module Main where

import Data.List (sort)
import Data.List.Split (splitOn)

solve :: String -> (Int, Int)
solve s = (s1, s2)
  where
    ok = (map (sum . map read) . splitOn [[]] . lines) s
    s1 = maximum ok
    s2 = (sum . take 3 . reverse . sort) ok

main :: IO ()
main = do
  myFile <- readFile "./input.txt"
  print $ solve myFile