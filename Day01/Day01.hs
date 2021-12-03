module Day01 where

import Control.Applicative (ZipList (..), getZipList)
import Data.List (tails)

parse :: String -> [Int]
parse = map read . lines

part1 :: [Int] -> Int
part1 depths = length . filter (== True) $ zipWith (<) depths (drop 1 depths)

part2 :: [Int] -> Int
part2 depths = part1 depths'
  where
    depths' = map sum (windows 3 depths)

    windows :: Int -> [Int] -> [[Int]]
    windows m = transpose' . take m . tails
      where
        transpose' :: [[a]] -> [[a]]
        transpose' = getZipList . traverse ZipList

-- A neater solution, observing that, when comparing the sums of the slidng
-- window, only the first element of the first window, and the last element of
-- the second, matter
part2' :: [Int] -> Int
part2' depths = length . filter (== True) $ zipWith (<) depths (drop 3 depths)

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
