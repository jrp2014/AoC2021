module Day08.Day08 where

-- https://github.com/AshleyYakeley/AdventOfCode/blob/master/app/2021/08/Main.hs

import Data.List (sort)
import Data.List.Split (splitOn)
import Data.Maybe (fromMaybe)
import Data.Set (Set)
import qualified Data.Set as Set

type Segments = String

type Evidence = [Segments] -- always 10

type Display = (Evidence, [Segments])

-- the signature of a particular segment, the lengths of the strings
-- in which it appears
type SegSignature = [Int]

getSegSignature :: Evidence -> Char -> SegSignature
getSegSignature ev c = sort $ length <$> filter (elem c) ev

-- the signature of a string of segments
type Signature = Set SegSignature

getSignature :: Evidence -> String -> Signature
getSignature ev s = Set.fromList $ fmap (getSegSignature ev) s

segsKnown :: Segments -> Bool
segsKnown ss = case length ss of
  2 -> True
  3 -> True
  4 -> True
  7 -> True
  _ -> False

reference :: Evidence
reference =
  ["abcefg", "cf", "acdeg", "acdfg", "bcdf", "abdfg", "abdefg", "acf", "abcdefg", "abcdfg"]

referenceTable :: [(Signature, Int)]
referenceTable = zip (fmap (getSignature reference) reference) [0 .. 9]

calcDisplay :: Display -> Int
calcDisplay (ev, ss) =
  segsToDigit (ss !! 0) * 1000
    + segsToDigit (ss !! 1) * 100
    + segsToDigit (ss !! 2) * 10
    + segsToDigit (ss !! 3)
  where
    segsToDigit :: Segments -> Int
    segsToDigit segs =
      fromMaybe (error "can't find signature in reference table") $
        lookup (getSignature ev segs) referenceTable

parse :: String -> [Display]
parse s = getDisplay <$> lines s
  where
    getDisplay :: String -> Display
    getDisplay ss = (words evidence, words segs)
      where
        [evidence, segs] = splitOn "|" ss

part1 :: [Display] -> Int
part1 displays = sum $ length . filter segsKnown . snd <$> displays

part2 :: [Display] -> Int
part2 displays = sum $ calcDisplay <$> displays

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
