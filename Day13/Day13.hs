module Day13.Day13 where

-- https://github.com/glguy/advent2021/blob/main/execs/Day13.hs

import Data.Bifunctor
import Data.List.Split (splitOn)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Foldable (toList)


type Coord = (Int, Int)

data Fold = Ax Int | Ay Int deriving (Show)

type Input = ([Fold], Set Coord)

parse :: String -> Input
parse s = (map parseFold (lines axes), Set.fromList $ map parseCoord (lines coords))
  where
    [coords, axes] = splitOn "\n\n" s

    parseCoord :: String -> Coord
    parseCoord = toyx . map read . splitOn ","
      where
        toyx [x, y] = (y, x)

    parseFold :: String -> Fold
    parseFold t
      | a == "x" = (Ax . read) v
      | a == "y" = (Ay . read) v
      where
        [a, v] = splitOn "=" $ last $ words t

-- | 2-dimensional fold the set of points over a line.
foldPoints :: Fold -> Set Coord -> Set Coord
foldPoints (Ax lx) = Set.map $ second (fold1 lx)
foldPoints (Ay ly) = Set.map $ first (fold1 ly)

-- | 1-dimensional fold updating one point
fold1 ::
  -- | fold
  Int ->
  -- | point
  Int ->
  Int
fold1 a i = a - abs (a - i)
solve :: Input -> [Set Coord]

solve (folds, pointSet) = scanl (flip foldPoints) pointSet folds

part1 :: Input -> Int
part1 i = length (solve i !! 1)

part2 :: Input -> String
part2 i = drawCoords (Map.fromSet (const 'X') (last $ solve i))

drawCoords :: Map Coord Char -> String
drawCoords pixels =
  case boundingBox (Map.keys pixels) of
    Nothing -> ""
    Just ((miny, minx), (maxy, maxx)) ->
      unlines [[Map.findWithDefault ' ' (y, x) pixels | x <- [minx .. maxx]] | y <- [miny .. maxy]]

boundingBox :: Foldable t => t Coord -> Maybe (Coord, Coord)
boundingBox t =
  case toList t of
    [] -> Nothing
    (y, x) : cs -> go y x y x cs
  where
    go loy lox hiy hix [] = Just ((loy, lox), (hiy, hix))
    go loy lox hiy hix ((y, x) : cs) = go (min loy y) (min lox x) (max hiy y) (max hix x) cs

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

  putStrLn "Test Part 2: "
  putStrLn $ part2 ptinput

  putStrLn "Part 2: "
  putStrLn $ part2 pinput
