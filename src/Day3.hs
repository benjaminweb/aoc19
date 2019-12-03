{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}

module Day3 where

import qualified Data.ByteString.Char8 as B8
import Data.List (sortOn)
import Data.List.Split (divvy)
import Data.Maybe (catMaybes, listToMaybe, mapMaybe)
import Text.Read (readMaybe)
import Prelude hiding (Left, Right)

type Distance = Int

type Steps = Int

data Direction = Up | Down | Right | Left deriving (Show)

data Instruction = Instruction Direction Steps deriving (Show)

data Coord = Coord Int Int deriving (Show, Eq, Ord)

type Line = (Coord, Coord)

data Axis = Horizontal | Vertical | Halt deriving (Show, Eq)

data Side = Above | Equal | Below deriving (Show, Eq)

getInput :: FilePath -> IO [[Instruction]]
getInput = (map parseInstructions . B8.lines <$>) . B8.readFile

parseInstructions :: B8.ByteString -> [Instruction]
parseInstructions = mapMaybe parseInstruction . B8.split ','

-- | ByteString to Instruction.
--
-- >>> parseInstruction "R75"
-- Just (Right 75)
-- >>> parseInstruction "D30"
-- Just (Down 30)
-- >>> parseInstruction "R83"
-- Just (Right 83)
---- >>> parseInstruction "L12"
-- Just (Left 12)
parseInstruction :: B8.ByteString -> Maybe Instruction
parseInstruction (B8.uncons -> Just (x, xs)) = Instruction (toDirection x) . fst <$> B8.readInt xs

toDirection :: Char -> Direction
toDirection 'U' = Up
toDirection 'D' = Down
toDirection 'L' = Left
toDirection 'R' = Right
toDirection x = error $ x : " is invalid direction specifier, allowed are only: U, D, L, R"

move :: Coord -> Instruction -> Coord
move (Coord x y) (Instruction Up d) = Coord (x + d) y
move (Coord x y) (Instruction Down d) = Coord (x - d) y
move (Coord x y) (Instruction Right d) = Coord x (y + d)
move (Coord x y) (Instruction Left d) = Coord x (y - d)

-- | Coords on the line between two Coords.
--
-- >>> line (Coord 0 0, Coord 0 2)
-- [Coord 0 0, Coord 0 1, Coord 0 2]
line :: Line -> [Coord]
line (Coord x1 y1, Coord x2 y2) = [Coord x y | x <- [min x1 x2 .. max x1 x2], y <- [min y1 y2 .. max y1 y2]]

-- | Returns list of turning points on the grid.
edges :: [Instruction] -> [Coord]
edges = scanl move (Coord 0 0)

-- | Determine shortest taxicab distance between two Coords.
--
-- >>> taxicabDistance (Coord 0 0) (Coord 5 5)
-- 10
--
-- >>> taxicabDistance (Coord 1 1) (Coord 5 5)
-- 8
taxicabDistance :: Coord -> Coord -> Distance
taxicabDistance (Coord x1 y1) (Coord x2 y2) = abs (x1 - x2) + abs (y1 - y2)

axis :: Line -> Axis
axis (Coord x1 y1, Coord x2 y2)
  | x1 == x2 && y1 /= y2 = Vertical
  | x1 /= x2 && y1 == y2 = Horizontal
  | otherwise = Halt

intersect :: Line -> Line -> Maybe Coord
intersect a b = let op = listToMaybe [x | x <- line b, x `elem` line a] in
  case (axis a, axis b) of
    (Horizontal, Vertical)
      | withinReach a b && changedSides a b -> op
      | otherwise -> Nothing
    (Vertical, Horizontal)
      | withinReach a b && changedSides a b -> op
      | otherwise -> Nothing
    (Vertical, Vertical) -> Nothing
    (Horizontal, Horizontal) -> Nothing
    (_, _) -> Nothing

withinReach :: Line -> Line -> Bool
withinReach (Coord x1 y1, Coord x1' y1') (Coord x2 y2, Coord x2' y2') = within x2 x2' x1 || within x2 x2' x1' || within y1 y1' y2 || within y1 y1' y2'

-- |
--
-- >>> side (Coord 0 0, Coord 0 10) (Coord 10 12)
-- Above
-- >>> side (Coord 0 0, Coord 0 10) (Coord (-1) 12)
-- Below
side :: Line -> Coord -> Side
side (Coord x1 y1, Coord x1' y1') (Coord x2 y2)
  | x1 == x1' && x2 > x1 = Above
  | x1 == x1' && x2 < x1 = Below
  | x1 == x1' && x2 == x2 = Equal
  | y1 == y1' && y2 > y1 = Above
  | y1 == y1' && y2 < y1 = Below
  | y1 == y1' && y2 == y1 = Equal
  | otherwise = error "only horizontal and vertical lines supported!"

-- | To intersect, one line must change the side of the axis formed by the other.
--
-- >>> changedSides (Coord 0 0, Coord 0 10) (Coord 0 2, Coord 5 2)
-- True
-- >>> changedSides (Coord 0 0, Coord 0 10) (Coord 1 2, Coord 5 2)
-- False
changedSides :: Line -> Line -> Bool
changedSides a b = side a (fst b) /= side a (snd b)

-- | is x between a and b?
within :: Int -> Int -> Int -> Bool
within a b x = (a <= x && x <= b) || (b <= x && x <= a)

-- | Edges to Line segments.
lines' :: [Coord] -> [Line]
lines' xs = zip xs $ tail xs

-- | Core logic: For two wires, find intersection point with minimal distance to control port.
--
-- >>> let [a, b] = map parseInstructions ["R75,D30,R83,U83,L12,D49,R71,U7,L72","U62,R66,U55,R34,D71,R55,D58,R83"] in nearestIntersection a b
-- Just 159
-- >>> let [a, b] = map parseInstructions ["R98,U47,R26,D63,R33,U87,L62,D20,R33,U53,R51","U98,R91,D20,R16,D67,R40,U7,R15,U6,R7"] in nearestIntersection a b
-- Just 135
nearestIntersection :: [Instruction] -> [Instruction] -> Maybe Distance
nearestIntersection a b = snd <$> listToMaybe (sortOn snd [(z, taxicabDistance (Coord 0 0) z) | z <- catMaybes [x `intersect` y | x <- a', y <- b'], z /= Coord 0 0])
  where
    [a', b'] = map (lines' . edges) [a, b]
