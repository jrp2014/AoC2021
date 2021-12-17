module Day17.Day17 where

-- https://github.com/glguy/advent2021/blob/main/execs/Day17.hs

import Data.Char (isDigit)
import Data.List (sort)

type Coord = (Int, Int) -- (y, x)

type TargetArea = (Coord, Coord) -- ((yMin, xMin), (yMax, xMax))

-- | The state of a traveling probe
data Probe
  = -- | x-position y-position x-velocity y-velocity
    P !Int !Int !Int !Int
  deriving (Show)

parse :: String -> TargetArea
parse input = ((yMin, xMin), (yMax, xMax))
  where
    clean k = if isDigit k || k == '-' then k else ' '

    [x1, x2, y1, y2] = map read . words $ map clean input

    [xMin, xMax] = sort [x1, x2]
    [yMin, yMax] = sort [y1, y2]


-- | Advance the probe one timestep
step :: Probe -> Probe
step (P x y vx vy) = P (x + vx) (y + vy) (vx - signum vx) (vy - 1)

-- | Run a simulation returning the maximum height seen if
-- the probe ever succeeds in hitting the target.
--
-- >>> sim 20 30 (-10) (-5) 6 9
-- Just 45
sim ::
  -- | target x lo
  Int ->
  -- | target x hi
  Int ->
  -- | target y lo
  Int ->
  -- | target y hi
  Int ->
  -- | initial x velocity
  Int ->
  -- | initial y velocity
  Int ->
  -- | maximum height if successful
  Maybe Int
sim xlo xhi ylo yhi vx0 vy0 = go 0 (P 0 0 vx0 vy0)
  where
    go best p@(P x y _ _)
      | y < ylo || x > xhi = Nothing -- too far
      | xlo <= x, x <= xhi, ylo <= y, y <= yhi = Just best
      | otherwise = go (max y best) (step p)

solve :: TargetArea -> [Int]
solve ((ylo, xlo), (yhi, xhi))
  | xlo >= 0 && yhi <= 0 =
      [y | vx <- [0 .. xhi], vy <- [ylo .. -ylo], Just y <- [sim xlo xhi ylo yhi vx vy]]

part1 :: TargetArea -> Int
part1 ta = maximum (solve ta)

part2 :: TargetArea -> Int
part2 ta = length (solve ta)

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
