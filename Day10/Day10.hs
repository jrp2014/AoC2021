module Day10.Day10 where

-- https://github.com/sharno/AdventOfCode2021-Hs/blob/main/Day10.hs

import Data.List (foldl', sort)

openBraces :: [Char]
openBraces = "([{<"

closeBraces :: [Char]
closeBraces = ")]}>"

closes :: Char -> Char -> Bool
closes c o = case lookup c bracePairs of
  Just o' -> o == o'
  Nothing -> error "corrupt input"
  where
    bracePairs = zip closeBraces openBraces

parse :: String -> [String]
parse = map parseLine . lines

parseLine :: [Char] -> [Char]
parseLine = foldl' go []
  where
    go :: [Char] -> Char -> [Char]
    go [c] _ | c `elem` closeBraces = [c] -- keep unmatched closing brace
    go xs o | o `elem` openBraces = o : xs -- stack opening brace
    go (o : xs) c | c `closes` o = xs -- pop matching brace
    go _ c = [c] -- illegal character

part1 :: [String] -> Int
part1 = sum . map score
  where
    score :: [Char] -> Int
    score ")" = 3
    score "]" = 57
    score "}" = 1197
    score ">" = 25137
    score _ = 0

part2 :: [String] -> Int
part2 = middle . sort . filter (> 0) . map score2
  where
    score2 :: [Char] -> Int
    score2 = foldl' score 0
      where
        score :: Int -> Char -> Int
        score acc '(' = acc * 5 + 1
        score acc '[' = acc * 5 + 2
        score acc '{' = acc * 5 + 3
        score acc '<' = acc * 5 + 4
        score _ _ = 0

    middle :: [Int] -> Int
    middle xs = xs !! (length xs `div` 2)

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
