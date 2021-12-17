module Day14.Day14 where

--

import Data.List.Split ( splitOn )
import qualified Data.Map as Map

type Polymer = Map.Map (Char, Char) Int

type Rules = Map.Map (Char, Char) Char

type Input = (Polymer, Rules)

parse :: String -> Input
parse input =
  ( foldr (\p -> Map.insertWith (+) p 1) Map.empty (zip template (tail template)),
    Map.fromList $
      (\[[x, y], [rhs]] -> ((x, y), rhs)) . splitOn " -> " <$> insertions
  )
  where
    [[template], insertions] = lines <$> splitOn "\n\n" input

step :: Rules -> Polymer -> Polymer
step rules =
  Map.foldrWithKey
    ( \(x, y) n ->
        let p = rules Map.! (x, y)
         in Map.insertWith (+) (x, p) n . Map.insertWith (+) (p, y) n
    )
    Map.empty

-- Won't work if first and last are the same
counts :: Polymer -> Map.Map Char Int
counts =
  Map.map (\n -> (n + 1) `quot` 2)
--  Map.map (\n -> (n + 1) `div` 2)
    . Map.foldrWithKey
      (\(x, y) n -> Map.insertWith (+) x n . Map.insertWith (+) y n)
      Map.empty

solve :: Int -> Rules -> Polymer -> Int
solve n rules polymer = maximum cs - minimum cs
  where
    cs :: Map.Map Char Int
    cs = counts $ iterate (step rules) polymer !! n

part1 :: Input -> Int
part1 (polymer, rules) = solve 10 rules polymer

part2 :: Input -> Int
part2 (polymer, rules) = solve 40 rules polymer

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
