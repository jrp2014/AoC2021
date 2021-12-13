module Day12.Day12 where

import Data.List.Split (splitOn)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set

type Cave = String

type Path = [String]

type Paths = [Path]

type Caves = Map Cave Path

parse :: String -> Caves
parse = adjacents . fmap (splitOn "-") . lines
  where
    adjacents :: Paths -> Caves
    adjacents = foldr (\[a, b] -> Map.insertWith (++) b [a] . Map.insertWith (++) a [b]) Map.empty

walk :: Caves -> (Set Cave, Bool) -> Cave -> Int
walk caves visit cave =
  if cave == "end"
    then 1
    else maybe 0 (sum . flip map (caves Map.! cave) . walk caves) $ step visit cave

step :: (Set Cave, Bool) -> Cave -> Maybe (Set Cave, Bool)
step (visit, twice) cave
  | small cave =
      if cave `Set.notMember` visit
        then if twice && cave /= "start" then Just (visit, False) else Nothing
        else Just (Set.delete cave visit, twice)
  | otherwise = Just (visit, twice)

small :: Cave -> Bool
small = (> 'Z') . head

solve :: Caves -> Bool -> Int
solve caves twice = walk caves (Set.filter small (Map.keysSet caves), twice) "start"

part1 :: Caves -> Int
part1 caves = solve caves False

part2 :: Caves -> Int
part2 caves = solve caves True

main :: IO ()
main = do
  putStr "Test Part 1: "
  tinput <- readFile "test.txt"
  let ptinput = parse tinput
  print $ part1 ptinput

  putStr "Test Part 1: "
  tinput2 <- readFile "test2.txt"
  let ptinput2 = parse tinput2
  print $ part1 ptinput2

  putStr "Test Part 1: "
  tinput3 <- readFile "test3.txt"
  let ptinput3 = parse tinput3
  print $ part1 ptinput3

  putStr "Part 1: "
  input <- readFile "input.txt"
  let pinput = parse input
  print $ part1 pinput

  putStr "Test Part 2: "
  print $ part2 ptinput

  putStr "Test Part 2: "
  print $ part2 ptinput2

  putStr "Test Part 2: "
  print $ part2 ptinput3

  putStr "Part 2: "
  print $ part2 pinput
