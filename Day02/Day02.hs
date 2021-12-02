module Day02 where

import Data.Foldable (foldl')

data Position = Position Int Int -- horizontal position, depth
  deriving (Show, Eq)

origin :: Position
origin = Position 0 0

data Command = Forward Int | Down Int | Up Int

type Course = [Command]

parse :: String -> Course
parse = map parseCommand . lines
  where
    parseCommand :: String -> Command
    parseCommand s = case words s of
      ["forward", units] -> Forward (read units)
      ["down", units] -> Down (read units)
      ["up", units] -> Up (read units)

part1 :: Course -> Int
part1 = answer . foldl' step origin
  where
    step :: Position -> Command -> Position
    step (Position h d) (Forward units) = Position (h + units) d
    step (Position h d) (Down units) = Position h (d + units)
    step (Position h d) (Up units) = Position h (d - units)

    answer :: Position -> Int
    answer (Position h d) = h * d


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

{-

  putStr "Test Part 2: "
  print $ part2 ptinput

  putStr "Part 2: "
  print $ part2 pinput

  -}
