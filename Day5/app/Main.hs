module Main where

import Data.Char (isDigit)
import Data.List (transpose)
import Data.List.Split (chunksOf, splitOn)

getStack :: [String] -> [String]
getStack = map (filter (/= ' ')) . transpose . take 8 . map (map (!! 1) . chunksOf 4)

getMove :: String -> [Int]
getMove = map read . filter (not . null) . splitOn " " . filter (\c -> isDigit c || c == ' ')

solve' :: (String -> String) -> [String] -> [Int] -> [String]
solve' f ss [n, from, to] = next
  where
    elts = take n (ss !! (from - 1))
    removed = drop n (ss !! (from - 1))
    to' = f elts ++ (ss !! (to - 1))
    next' = take (from - 1) ss ++ removed : drop from ss
    next = take (to - 1) next' ++ to' : drop to next'

main :: IO ()
main = do
  myFile <- readFile "input.txt"
  let ss = getStack (lines myFile)
  let cms = map getMove ((drop 10 . lines) myFile)
  let resf f = map head $ foldl (solve' f) ss cms
  print (resf reverse, resf id)