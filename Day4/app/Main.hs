module Main where

import Data.List.Split (splitOneOf)

solve :: String -> (Int, Int)
solve s =
  (ct all, ct any)
  where
    [a, b, c, d] = (map read . splitOneOf ",-") s :: [Int]
    l1 = [a .. b]
    l2 = [c .. d]
    ct f = if f (`elem` l2) l1 || f (`elem` l1) l2 then 1 else 0

main :: IO ()
main = do
  myFile <- readFile "input.txt"
  print $ (foldl (\(a, b) (x, y) -> (a + x, b + y)) (0, 0) . map solve . lines) myFile