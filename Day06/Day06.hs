{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Day06.Day06 where

-- https://github.com/glguy/advent2021/blob/main/execs/Day06.hs

import Data.MultiSet (MultiSet)
import qualified Data.MultiSet as MultiSet

newtype Timer = Timer Int deriving (Show, Eq, Ord, Num)

type FrequencyMap = MultiSet Timer

parse :: String -> FrequencyMap
parse s = MultiSet.fromList $ Timer <$> read ('[' : s ++ "]")

step :: FrequencyMap -> FrequencyMap
step = MultiSet.concatMap tick
  where
    tick :: Timer -> [Timer]
    tick 0 = [6, 8]
    tick timer = [timer - 1]

part1 :: FrequencyMap -> Int
part1 i = MultiSet.size $ iterate step i !! 80

part2 :: FrequencyMap -> Int
part2 i = MultiSet.size $ iterate step i !! 256

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
