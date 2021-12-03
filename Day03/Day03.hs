module Day03 where

import Data.List (foldl', transpose)

data Bit = Zero | One deriving (Eq, Show)

type Bits = [Bit]

count :: (a -> Bool) -> [a] -> Int
count p = length . filter p

bitsToDecimal :: Bits -> Int
bitsToDecimal = foldl' (\acc b -> 2 * acc + if b == Zero then 0 else 1) 0

complement :: Bit -> Bit
complement One = Zero
complement Zero = One

moreCommon :: Bits -> Bit
moreCommon bits
  | count (== Zero) bits <= count (== One) bits = One
  | otherwise = Zero

lessCommon :: Bits -> Bit
lessCommon = complement . moreCommon

parse :: String -> [Bits]
parse = map (map charToBit) . lines
  where
    charToBit :: Char -> Bit
    charToBit '0' = Zero
    charToBit '1' = One
    charToBit x = error $ x : " is not 0 or 1"

chooseByCol :: (Bits -> Bit) -> [Bits] -> Bits
chooseByCol chooser = map chooser . transpose

gamma :: [Bits] -> Int
gamma = bitsToDecimal . chooseByCol moreCommon

epsilon :: [Bits] -> Int
epsilon = bitsToDecimal . chooseByCol lessCommon

part1 :: [Bits] -> Int
part1 bits = gamma bits * epsilon bits

filterByCol :: (Bits -> Bit) -> [Bits] -> Bits
filterByCol _ [bits] = bits
filterByCol chooser bitss =
  chosenBits : filterByCol chooser [bitss' | bits : bitss' <- bitss, chosenBits == bits]
  where
    chosenBits = chooser [bits | bits : _ <- bitss]

o2 :: [Bits] -> Int
o2 = bitsToDecimal . filterByCol moreCommon

co2 :: [Bits] -> Int
co2 = bitsToDecimal . filterByCol lessCommon

part2 :: [Bits] -> Int
part2 bits = o2 bits * co2 bits

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
