module Day25.Day25 where

-- https://github.com/glguy/advent2021/blob/main/execs/Day25.hs

import Data.Map (Map)
import qualified Data.Map.Strict as Map

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
step ny nx = step1 'v' below . step1 '>' right
  where
    -- Increment y coordinate
    below :: Coord -> Coord
    below (C y x) = C ((y + 1) `mod` ny) x

    -- Increment x coordinate
    right :: Coord -> Coord
    right (C y x) = C y ((x + 1) `mod` nx)

    step1 :: Char -> (Coord -> Coord) -> Grid -> Grid
    step1 c delta grid =
      Map.fromList
        [ (if v == c && Map.notMember k' grid then k' else k, v)
          | (k, v) <- Map.toList grid,
            let k' = delta k
        ]

-- | Render a minimal bounding box containing all the characters
-- at the given coordinates. Empty space filled with dots.
drawPicture :: Grid -> String
drawPicture pixels =
  case boundingBox (Map.keys pixels) of
    Nothing -> ""
    Just (C miny minx, C maxy maxx) ->
      unlines [[Map.findWithDefault '.' (C y x) pixels | x <- [minx .. maxx]] | y <- [miny .. maxy]]
  where
    -- Find the upper-left and lower-right coordinates that
    -- inclusively contain all the coordinates in a list of
    -- coordinates.
    boundingBox :: [Coord] -> Maybe (Coord, Coord)
    boundingBox [] = Nothing
    boundingBox (C y x : cs) = go y x y x cs
      where
        go :: Int -> Int -> Int -> Int -> [Coord] -> Maybe (Coord, Coord)
        go loy lox hiy hix [] = Just (C loy lox, C hiy hix)
        go loy lox hiy hix (C y x : cs) = go (min loy y) (min lox x) (max hiy y) (max hix x) cs

part1 :: Grid -> Int
part1 grid = length (evolution steps)
  where
    coords = Map.keys grid
    ny = 1 + maximum [y | C y _ <- coords]
    nx = 1 + maximum [x | C _ x <- coords]
    grid' = Map.filter (`elem` ">v") grid

    steps :: [Grid]
    steps = iterate (step ny nx) grid'

part2 :: Grid -> Int
part2 grid = undefined

main :: IO ()
main = do
  putStr "Test Part 1: "
  tgridut <- readFile "test.txt"
  let ptgridut = parse tgridut
  print $ part1 ptgridut

  putStr "Part 1: "
  gridut <- readFile "input.txt"
  let pgridut = parse gridut
  print $ part1 pgridut

  putStr "Test Part 2: "
  print $ part2 ptgridut

  putStr "Part 2: "
  print $ part2 pgridut
