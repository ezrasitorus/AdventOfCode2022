module Main where

import Data.List (nub)

solve :: [String] -> (Int, Int) -> (Int, Int) -> [(Int, Int)] -> [(Int, Int)]
solve [] _ _ is = is
solve ((c : cs) : ss) h@(x, y) t@(u, v) is = solve ss h' (last far) (is ++ close' ++ far)
  where
    n = read cs :: Int
    (movement, [h']) =
      splitAt
        n
        ( case c of
            'R' -> [(x', y) | x' <- [x .. x + n]]
            'L' -> [(x', y) | x' <- [x, x - 1 .. x - n]]
            'U' -> [(x, y') | y' <- [y .. y + n]]
            'D' -> [(x, y') | y' <- [y, y - 1 .. x - n]]
        )
    f2 (u, v) (a, b) = abs (a - u) + abs (b - v) < 2
    (close, far) = span (f2 t) movement
    close' = if f2 (last close) (head far) then [] else (take 1 . reverse) close

main :: IO ()
main = do
  cmds <- readFile "input.txt"
  print $ solve (lines cmds) (0, 0) (0, 0) []
