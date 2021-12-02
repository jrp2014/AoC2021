module Day02 where

import Data.Foldable (foldl')

data Position = Position Int Int Int -- horizontal position, depth, aim
  deriving (Show, Eq)

origin :: Position
origin = Position 0 0 0

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
      _ -> error "Parse error"

part1 :: Course -> Int
part1 = answer . foldl' step origin
  where
    step :: Position -> Command -> Position
    step (Position h d _) (Forward x) = Position (h + x) d 0
    step (Position h d _) (Down x) = Position h (d + x) 0
    step (Position h d _) (Up x) = Position h (d - x) 0

part2 :: Course -> Int
part2 = answer . foldl' step origin
  where
    step :: Position -> Command -> Position
    step (Position h d a) (Forward x) = Position (h + x) (d + a * x) a
    step (Position h d a) (Down x) = Position h d (a + x)
    step (Position h d a) (Up x) = Position h d (a - x)

answer :: Position -> Int
answer (Position h d _) = h * d

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
