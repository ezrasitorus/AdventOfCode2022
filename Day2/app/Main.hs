module Main where

import Data.Maybe (fromJust)

solve :: String -> (Int, Int)
solve = foldl (\(a, b) (x, y) -> (a + x, b + y)) (0, 0) . map (\[e, ' ', p] -> solve' (e, p)) . lines

solve' :: (Char, Char) -> (Int, Int)
solve' (e, p) = (p1 + 3 * ((p1 + 1 - e') `mod` 3), (3 * p2) + (3 - ((1 - p2 - e') `mod` 3)))
  where
    e' = (fromJust . lookup e) [('A', 1), ('B', 2), ('C', 3)]
    p1 = (fromJust . lookup p) [('X', 1), ('Y', 2), ('Z', 3)]
    p2 = (fromJust . lookup p) [('X', 0), ('Y', 1), ('Z', 2)]

main :: IO ()
main = do
  myFile <- readFile "./input.txt"
  print $ solve myFile
