{-# LANGUAGE TupleSections #-}

module Day11.Day11 where

-- https://github.com/glguy/advent2021/blob/main/execs/Day11.hs

import Data.Char (digitToInt)
import Data.List (foldl', elemIndex)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (fromJust)

type Energy = Int

type Coord = (Int, Int) -- (y, x)

type EnergyLevelMap = Map Coord Energy

neighbours :: Coord -> [Coord]
neighbours (y, x) =
  [ (y, x - 1),
    (y + 1, x),
    (y, x + 1),
    (y - 1, x),
    (y - 1, x - 1),
    (y - 1, x + 1),
    (y + 1, x - 1),
    (y + 1, x + 1)
  ]

count :: Foldable t => (a -> Bool) -> t a -> Int
count p = foldl' (\acc x -> if p x then acc + 1 else acc) 0

parse :: String -> EnergyLevelMap
parse s =
  Map.fromList
    [ ((y, x), digitToInt z)
      | (y, xs) <- zip [0 ..] (lines s),
        (x, z) <- zip [0 ..] xs
    ]

-- | Initial grid state to flashes per step
simulate :: EnergyLevelMap -> [Energy]
simulate = fmap (count (0 ==)) . tail . iterate step

-- | Advance the state of the world one time step
step :: EnergyLevelMap -> EnergyLevelMap
step m = foldl excite (fmap (1 +) m) [k | (k, 9) <- Map.toList m]

-- | Excite an octopus at the given location
excite :: EnergyLevelMap -> Coord -> EnergyLevelMap
excite m x =
  case Map.lookup x m of
    Just e
      | e >= 9 -> foldl excite (Map.insert x 0 m) (neighbours x)
      | e >= 1 -> Map.insert x (1 + e) m
    _ -> m

part1 :: EnergyLevelMap -> Int
part1 m = sum (take 100 flashes)
  where
    flashes :: [Energy]
    flashes = simulate m

part2 :: EnergyLevelMap -> Int
part2 m = 1 + fromJust (elemIndex (Map.size m) flashes)
  where
    flashes :: [Energy]
    flashes = simulate m

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
