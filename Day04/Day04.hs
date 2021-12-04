module Day04.Day04 where

-- https://gitlab.com/sakisan/adventofcode/-/blob/2021/Haskell/Day04.hs

import Data.List (find, inits, partition, sort, transpose)
import Data.List.Split (splitOn)
import Data.Maybe (mapMaybe)

type Draws = [Int]

type Row = [Int]

type Card = [Row]

type Input = (Draws, [Card])

isWinningCard :: Card -> Draws -> Bool
isWinningCard card draws = complete card || complete (transpose card)
  where
    complete = any (all (`elem` draws))

score :: Card -> Draws -> Int
score card draws = last draws * sum unmarked
  where
    (_marked, unmarked) = partition (`elem` draws) (concat card)

findWin :: Draws -> Card -> Maybe (Int, Int)
findWin draws card =
  (,) <$> length <*> score card
    <$> find (isWinningCard card) (inits draws)

parse :: String -> Input
parse s = (map read $ splitOn "," rawDraws, map parseCard $ splitOn [""] rawCards)
  where
    (rawDraws : rawCards) = lines s
    parseCard = map (map read . words)

pickCard :: ([(Int, Int)] -> (Int, Int)) -> Input -> Int
pickCard f (draws, cards) = snd . f . sort $ mapMaybe (findWin draws) cards

part1 :: Input -> Int
part1 = pickCard head

part2 :: Input -> Int
part2 = pickCard last

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
