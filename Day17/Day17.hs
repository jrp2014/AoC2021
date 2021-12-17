module Day17.Day17 where

import Data.Char (isDigit)
import Data.List (sort)

type Coord = (Int, Int) -- (y, x)

type TargetArea = (Coord, Coord) -- ((yMin, xMin), (yMax, xMax))

parse :: String -> TargetArea
parse input = ((yMin, xMin), (yMax, xMax))
  where
    clean k = if isDigit k || k == '-' then k else ' '

    [x1, x2, y1, y2] = map read . words $ map clean input

    [xMin, xMax] = sort [x1, x2]
    [yMin, yMax] = sort [y1, y2]

ys :: TargetArea -> Int -> [Int]
ys ((yMin, _), _) y'0 =
  takeWhile (>= yMin) $
    scanl (+) 0 $ iterate (\y -> y - 1) y'0

xs :: Int -> [Int]
xs x'0 = scanl (+) 0 $ iterate (\x -> x - signum x) x'0

xss :: TargetArea -> [[Int]]
xss (_, (_, xMax)) = xs <$> [0 .. xMax]

yss :: TargetArea -> [[Int]]
yss ta@((yMin, yMax), _) = [ys' | ys' <- ys ta <$> [yMin .. abs yMin], last ys' <= yMax]

inTargetArea :: TargetArea -> Coord -> Bool
inTargetArea ((yMin, xMin), (yMax, xMax)) (x, y) =
  y >= yMin && y <= yMax
    && x >= xMin
    && x <= xMax

trajectories :: TargetArea -> [[Coord]]
trajectories ta =
  [ trajectory | ys <- yss ta, xs <- xss ta, let trajectory = zip xs ys, any (inTargetArea ta) trajectory
  ]

part1 :: TargetArea -> Int
part1 ta = maximum . map snd $ last (trajectories ta)

part2 :: TargetArea -> Int
part2 ta = length $ trajectories ta

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
