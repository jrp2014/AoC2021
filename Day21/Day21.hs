module Day21.Day21 where

import Control.Monad
import qualified Data.Map as Map
import qualified Data.Bifunctor as Bifunctor

type Pos = Int

type Score = Int

-- * Parsing

parse :: String -> (Pos, Pos)
parse s = (p1, p2)
  where
    [p1, p2] = map (read . last . words) $ lines s

part1 :: (Pos, Pos) -> Int
part1 p = go 1 p (0, 0)
  where
    go :: Int -> (Pos, Pos) -> (Score, Score) -> Score
    go die (pos1, pos2) (score1, score2)
      | score1 >= 1000 = (die - 3 - 1) * (score2 - pos2) -- undo player 2's last turn
      | score2 >= 1000 = (die - 1) * score1
      | otherwise = go (die + 6) (pos1', pos2') (score1 + pos1', score2 + pos2')
      where
        pos1' = (pos1 + ((die + die + 1 + die + 2) `mod'` 100)) `mod'` 10
        pos2' = (pos2 + ((die + 3 + die + 4 + die + 5) `mod'` 100)) `mod'` 10

mod' :: Int -> Int -> Int
mod' a b = (a - 1) `mod` b + 1

part1' :: (Pos, Pos) -> Int
part1' p = go 0 p (0, 0)
  where
    go :: Int -> (Pos, Pos) -> (Score, Score) -> Score
    go die (pos1, pos2) (score1, score2)
      | score2 >= 1000 = die * score1
      | otherwise = go (die + 3) (pos2, pos1') (score2, score1 + pos1')
      where
        pos1' = (pos1 + 3 * die + 6) `mod'` 10

(#+) (a, b) (c, d) = (a + c, b + d)

(.*) f (a, b) = (f * a, f * b)

part2 :: (Pos, Pos) -> Int
part2 inp = undefined
  where
    go :: (Pos, Pos) -> (Score, Score) -> (Score, Score)
    go (pos1, pos2) (score1, score2)
      | score1 >= 21 = (1, 0)
      | otherwise =
          Bifunctor.bimap fst snd
            [ (n * w1, n * w2) | (move, n) <- freqMap, let pos1' = (pos1 + move) `mod'` 10, let (w2, w1) = go (pos2, pos1') (score2, score1 + pos1')
            ]
      where
        freqMap = [(3, 1), (4, 3), (5, 6), (6, 7), (7, 6), (8, 3), (9, 1)]

main :: IO ()
main = do
  putStr "Test Part 1: "
  tinput <- readFile "test.txt"
  let ptinput = parse tinput
  print $ part1 ptinput
  print $ part1' ptinput

  putStr "Part 1: "
  input <- readFile "input.txt"
  let pinput = parse input
  print $ part1 pinput
  print $ part1' pinput

  putStr "Test Part 2: "
  print $ part2 ptinput

  putStr "Part 2: "
  print $ part2 pinput
