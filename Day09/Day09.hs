{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}

module Day09.Day09 where

-- https://github.com/mstksg/advent-of-code-2021/blob/master/src/AOC/Challenge/Day09.hs

import Data.Char (digitToInt)
import Data.Foldable (toList)
import Data.List (find, sortBy)
import Data.Map (Map)
import qualified Data.Map as Map

type Height = Int

type Coord = (Int, Int) -- (y, x)

type HeightMap = Map Coord Height

neighbours :: Coord -> [Coord]
neighbours (y, x) =
  [ (y, x - 1),
    (y + 1, x),
    (y, x + 1),
    (y - 1, x)
  ]

parse :: String -> HeightMap
parse s =
  Map.fromList
    [ ((y, x), digitToInt (head h))
      | (y, xs) <- zip [0 ..] (map (map (: [])) $ lines s),
        (x, h) <- zip [0 ..] xs,
        h /= "9" -- a high point is never going to be lowest
    ]

-- points adjacent to c that are lower than h
findLows :: HeightMap -> [(Coord, Height)]
findLows hm = filter go . Map.toList $ hm
  where
    go (c, h) = all isLower (neighbours c)
      where
        isLower = maybe True (> h) . (`Map.lookup` hm)

part1 :: HeightMap -> Int
part1 = sum . map ((+ 1) . snd) . findLows

-- | Map of each point to the next point downhill.  If Nothing, then it's a low point.
flowMap :: HeightMap -> Map Coord (Maybe Coord)
flowMap hm = Map.mapWithKey go hm
  where
    go p h = find getGrad (neighbours p)
      where
        getGrad = maybe False (< h) . (`Map.lookup` hm)

part2 :: HeightMap -> Int
part2 hm = product . take 3 . sortBy (flip compare) . toList $ freqs res
  where
    -- map of points to their associated low points after flowing
    -- all the way downhill
    res :: Map Coord Coord
    res =
      Map.mapWithKey
        ( \p -> \case
            Nothing -> p
            Just q -> res Map.! q
        )
        (flowMap hm)

    -- \| Build a frequency map
    freqs :: (Foldable f, Ord a) => f a -> Map a Int
    freqs = Map.fromListWith (+) . map (,1) . toList

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
