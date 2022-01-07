{-# LANGUAGE TupleSections #-}

module Day19.Day19 where

-- https://github.com/glguy/advent2021/blob/main/execs/Day17.hs

import Data.Either (partitionEithers)
import Data.List (nub, permutations, transpose)
import Data.List.Split (splitOn)
import Data.Map (Map)
import qualified Data.Map as Map

-- * Parsing

parse :: String -> [Int]
parse = map (map parsePos . tail) . splitOn [""] . lines
  where
    parsePos = map read . splitOn ","

part1 :: [Int] -> Int
part1 inp = undefined

part2 :: [Int] -> Int
part2 inp = undefined

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
