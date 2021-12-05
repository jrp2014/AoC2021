{-# LANGUAGE ParallelListComp #-}

module Day05.Day05 where

-- https://github.com/glguy/advent2021/blob/main/execs/Day05.hs

import Data.List (group, sort)
import Data.List.Split (splitOn)

type Line = (Int, Int, Int, Int)

type Input = [Line]

parse :: String -> Input
parse = map (toLine . words) . lines
  where
    toLine :: [String] -> Line
    toLine [s1, "->", s2] = (x1, y1, x2, y2)
      where
        [x1, y1] = toPoint s1
        [x2, y2] = toPoint s2
    toPoint :: String -> [Int]
    toPoint s = map read $ splitOn "," s

count :: (a -> Bool) -> [a] -> Int
count p = length . filter p

range :: Int -> Int -> [Int]
range x y
  | x <= y = [x .. y]
  | otherwise = [x, x - 1 .. y]

points :: Line -> [(Int, Int)]
points (x1, y1, x2, y2)
  | x1 == x2 = [(x1, y) | y <- range y1 y2]
  | y1 == y2 = [(x, y1) | x <- range x1 x2]
  | otherwise = [(x, y) | x <- range x1 x2 | y <- range y1 y2]

isStraight :: Line -> Bool
isStraight (x1, y1, x2, y2) = x1 == x2 || y1 == y2

solve :: Input -> Int
solve = count (not . null . tail) . group . sort . concatMap points

part1 :: Input -> Int
part1 i = solve (filter isStraight i)

part2 :: Input -> Int
part2 = solve

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
