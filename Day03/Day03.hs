module Day03 where

import Data.Char (digitToInt)
import Data.List (transpose)

type Bit = Int

type Bits = [Bit]

parse :: String -> [Bits]
parse = map (map digitToInt) . lines

gamma' :: [Bits] -> Bits
gamma' bits = map (prevalentBit . sum) $ transpose bits
  where
    bitsLength :: Int
    bitsLength = length bits

    prevalentBit :: Int -> Bit
    prevalentBit bitsSum = if 2 * bitsSum > bitsLength then 1 else 0

bitsToDecimal :: Bits -> Int
bitsToDecimal = foldl (\acc b -> 2 * acc + b) 0

gamma :: [Bits] -> Int
gamma = bitsToDecimal . gamma'

complement :: Bits -> Bits
complement = map (\b -> if b == 0 then 1 else 0)

epsilon :: [Bits] -> Int
epsilon = bitsToDecimal . complement . gamma'

part1 :: [Bits] -> Int
part1 bits = gamma bits * epsilon bits

part2 :: [Bits] -> Int
part2 _ = 0

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
