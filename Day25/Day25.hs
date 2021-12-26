module Day25.Day25 where

-- https://github.com/glguy/advent2021/blob/main/execs/Day25.hs

import Data.Map (Map)
import qualified Data.Map.Strict as Map
import Data.Foldable


-- | A snailfish expression
data Coord = C !Int !Int -- y x
  deriving (Show, Eq, Ord)

type Grid = Map Coord Char

-- * Parsing

parse :: String -> Grid
parse = Map.fromList . coordLines . lines
  where
    coordLines :: [String] -> [(Coord, Char)]
    coordLines rows = [(C y x, z) | (y, row) <- zip [0 ..] rows, (x, z) <- zip [0 ..] row]

evolution :: Eq a => [a] -> [a]
evolution (x : y : _) | x == y = [x]
evolution (x : xs) = x : evolution xs
evolution [] = []

step :: Int -> Int -> Grid -> Grid
step ny nx = step1 ny nx 'v' below . step1 ny nx '>' right

-- | Increment y coordinate
below :: Coord -> Coord
below (C y x) = C (y + 1) x

-- | Increment x coordinate
right :: Coord -> Coord
right (C y x) = C y (x + 1)

step1 :: Int -> Int -> Char -> (Coord -> Coord) -> Grid -> Grid
step1 ny nx c f inp =
  Map.fromList
    [ (if v == c && Map.notMember k' inp then k' else k, v)
      | (k, v) <- Map.toList inp,
        let k' = fixup (f k)
    ]
  where
    fixup (C y x) = C (y `mod` ny) (x `mod` nx)

-- | Find the upper-left and lower-right coordinates that
-- inclusively contain all the coordinates in a list of
-- coordinates.
boundingBox :: [Coord] -> Maybe (Coord, Coord)
boundingBox t =
  case t of
    [] -> Nothing
    C y x : cs -> go y x y x cs
  where
    go loy lox hiy hix [] = Just (C loy lox, C hiy hix)
    go loy lox hiy hix (C y x : cs) = go (min loy y) (min lox x) (max hiy y) (max hix x) cs

-- | Render a minimal bounding box containing all the characters
-- at the given coordinates. Empty space filled with space characters.
drawPicture :: Grid -> String
drawPicture pixels =
  case boundingBox (Map.keys pixels) of
    Nothing -> ""
    Just (C miny minx, C maxy maxx) ->
      unlines [[Map.findWithDefault '.' (C y x) pixels | x <- [minx .. maxx]] | y <- [miny .. maxy]]

part1 :: Grid -> Int
part1 inp = length (evolution steps)
  where
    keys = Map.keys inp
    ny = 1 + maximum [y | C y _  <- keys ]
    nx = 1 + maximum [x | C _ x  <- keys ]
    inp' = Map.filter (`elem` ">v") inp
    steps :: [Grid]
    steps = iterate (step ny nx) inp'

part2 :: Grid -> Int
part2 inp = undefined

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
