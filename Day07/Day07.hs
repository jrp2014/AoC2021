module Day07.Day07 where

-- https://github.com/glguy/advent2021/blob/main/execs/Day07.hs

import Data.List (sort)

type Input = [Int]

parse :: String -> Input
parse s = read $ '[' : s ++ "]"

part1 :: Input -> Int
part1 i = sum [abs (x - median) | x <- i]
  where
    median :: Int
    median = sort i !! (length i `div` 2)

-- As the number of crabs becomes large, the optimal horizontal position tends
-- to their mean position (rather than their median, which was the case for part
-- 1).  But that may not be an integer, so just brute force
part2 :: Input -> Int
part2 i = minimum [sum [cost (abs (x - y)) | x <- i] | y <- [lo .. hi]]
  where
    lo = minimum i
    hi = maximum i
    cost d = d * (d + 1) `div` 2

-- The mean minimises the sum of squares
part2' :: Input -> Int
part2' i = sum [cost $ abs (x - mean) | x <- i]
  where
    mean :: Int
    mean = sum i `div` length i

    cost d = d * (d + 1) `div` 2

main :: IO ()
main = do
  putStr "Test Part 1: "
  tinput <- readFile "test.txt"
  let ptinput = parse tinput
  print $ part1 ptinput

  putStr "Part 1: "
  input <- readFile "input.txt"
  let pinput = parse input
  print $ part1 pinput

  putStr "Test Part 2: "
  print $ part2 ptinput

  putStr "Part 2: "
  print $ part2 pinput

  putStr "Part 2': "
  print $ part2' pinput
