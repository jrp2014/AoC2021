{-# LANGUAGE ViewPatterns #-}

module Day18.Day18 where

-- https://github.com/glguy/advent2021/blob/main/execs/Day17.hs

import Control.Applicative ((<|>))
import Data.Char (isDigit)
import Data.List (tails)
import Text.ParserCombinators.ReadP

-- | A snailfish expression
data X
  = -- | pair
    X :+ X
  | -- | regular number
    N Int

instance Show X where
  show (N x) = show x
  show (x :+ y) = '[' : show x ++ ", " ++ show y ++ "]"

-- * Parsing

parse :: String -> [X]
parse = map parseLine . lines

-- | Parse an expression
--
-- >>> parseLine "[[[[0,9],2],3],4]"
-- (((N 0 :+ N 9) :+ N 2) :+ N 3) :+ N 4
parseLine :: String -> X
parseLine (readP_to_S pList -> [(x, _)]) = x
parseLine _ = error "bad input"

-- | ReadP expression parser
pList :: ReadP X
pList =
  between (char '[') (char ']') ((:+) <$> pList <* char ',' <*> pList)
    +++ (N . read <$> munch1 isDigit)

-- * snailfish operations

-- | Add two expressions and reduce them
-- >>> parseLine "[[[[[9,8],1],2],3],4]"
-- [[[[0,9],2],3],4]
add :: X -> X -> X
add x y = reduce (x :+ y)

-- | Reduce an expression until it won't reduce
reduce :: X -> X
reduce x = maybe x reduce (explode x <|> split x)

-- | Replace the first pair of numbers at depth 4 with a @0@
-- and add the left and right components to the nearest number
-- on the left and right respectively.
explode :: X -> Maybe X
explode = go (4 :: Int) Top
  where
    go :: Int -> Zip -> X -> Maybe X
    go 0 z (N l :+ N r) = Just (fromZip (N 0) (addUpL l (addUpR r z)))
    go 0 _ _ = Nothing
    go d z (l :+ r) = go (d - 1) (ZL r z) l <|> go (d - 1) (ZR l z) r
    go _ _ _ = Nothing

-- | Replace the first number with value 10 or more with a pair
-- of it divided in half rounding first down then up.
split :: X -> Maybe X
split (N x)
  | x >= 10 = Just (N (x `div` 2) :+ N ((x + 1) `div` 2))
  | otherwise = Nothing
split (l :+ r) = (:+ r) <$> split l <|> (l :+) <$> split r

-- * Expression zippers

-- | The type of a hole in an expression tree. Values of
-- this type describe a location in an 'X' that's missing
-- a subterm. This term can be replaced with 'fromZip' giving
-- you the complete 'X' back.
data Zip
  = -- | The hole is on the right side of a pair
    ZR X Zip
  | -- | The hole is on the left side of a pair
    ZL X Zip
  | -- | the topmost hole
    Top
  deriving (Show)

-- | Rebuild an expression given a zipper and the value to put in the hole.
fromZip :: X -> Zip -> X
fromZip r (ZR l z) = fromZip (l :+ r) z
fromZip l (ZL r z) = fromZip (l :+ r) z
fromZip l Top = l

addUpL :: Int -> Zip -> Zip
addUpL _ Top = Top
addUpL n (ZR l z) = ZR (addDownR n l) z
addUpL n (ZL r z) = ZL r (addUpL n z)

addUpR :: Int -> Zip -> Zip
addUpR _ Top = Top
addUpR n (ZR l z) = ZR l (addUpR n z)
addUpR n (ZL r z) = ZL (addDownL n r) z

addDownL :: Int -> X -> X
addDownL n (l :+ r) = addDownL n l :+ r
addDownL n (N m) = N (n + m)

addDownR :: Int -> X -> X
addDownR n (l :+ r) = l :+ addDownR n r
addDownR n (N m) = N (n + m)

-- | Compute the /magnitude/ of an expression
--
-- >>> magnitude (head $ parse "[9,1]")
-- 29ß
-- >>> magnitude (head $ parse "[[1,2],[[3,4],5]]")
-- 143
-- >>> magnitude (parseLine "[[[[0,7],4],[[7,8],[6,0]]],[8,1]]")
-- 1384
-- >>> magnitude (parseLine "[[[[1,1],[2,2]],[3,3]],[4,4]]")
-- 445
-- >>> magnitude (parseLine "[[[[3,0],[5,3]],[4,4]],[5,5]]")
-- 791
-- >>> magnitude (parseLine "[[[[5,0],[7,4]],[5,5]],[6,6]]")
-- 1137
-- >>> magnitude (parseLine "[[[[8,7],[7,7]],[[8,6],[7,7]]],[[[0,7],[6,6]],[8,7]]]")
-- 3488
magnitude :: X -> Int
magnitude (N x) = x
magnitude (l :+ r) = 3 * magnitude l + 2 * magnitude r

part1 :: [X] -> Int
part1 inp = magnitude (foldl1 add inp)

part2 :: [X] -> Int
part2 inp =
  maximum
    [ magnitude (add x y) `max` magnitude (add y x)
      | x : ys <- tails inp,
        y <- ys
    ]

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
