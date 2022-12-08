module Main where

import Data.Char (ord)
import Data.List.Split (chunksOf)

solve :: String -> (Int, Int)
solve s = (m1, m2)
  where
    ss = lines s
    convert c = 1 + if ord c <= ord 'Z' then 26 + ord c - ord 'A' else ord c - ord 'a'
    m1 = foldl (part1 convert) 0 ss
    m2 = foldl (part2 convert) 0 (chunksOf 3 ss)

part1 :: (Char -> Int) -> Int -> String -> Int
part1 convert n s = n + convert c
  where
    len = length s
    (comp1, comp2) = splitAt (div len 2) s
    c = (head . filter (`elem` comp2)) comp1

part2 :: (Char -> Int) -> Int -> [String] -> Int
part2 convert n (s : ss) = n + convert c
  where
    c = (head . filter (\x -> all (elem x) ss)) s

main :: IO ()
main = do
  myFile <- readFile "./input.txt"
  print $ solve myFile