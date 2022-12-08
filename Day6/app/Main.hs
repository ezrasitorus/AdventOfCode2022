module Main where

import Data.List (nub)

solve :: String -> Int -> Int -> Int
solve ss n m = if length c == m then n else solve (tail ss) (n + 1) m
  where
    c = (nub . take m) ss

main :: IO ()
main = do
  myFile <- readFile "input.txt"
  print (solve myFile 4 4, solve myFile 14 14)
