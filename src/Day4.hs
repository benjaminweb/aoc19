module Day4 where

import Data.List (group)
import Data.List.Split

noDigits :: (RealFrac a, Integral p, Floating a) => a -> p
noDigits x
  | x < 10 = 1
  | x == 10 = 2
  | otherwise = ceiling $ logBase 10 x

-- | Dissect Int into list of its digits.
--
-- >>> digits 120
-- [1,2,0]
-- >>> digits 1
-- [1]
-- >>> digits 12
-- [1,2]
-- >>> digits 10
-- [1,0]
digits :: Int -> [Int]
digits z = reverse $ fst $ last $ take toTake $ iterate (\(x, y) -> (x ++ [mod y 10], div y 10)) ([], z)
  where
    toTake
      | noDigits (fromIntegral z) == 0 = 1
      | otherwise = noDigits (fromIntegral z) + 1

-- | Two adjacent numbers must be the same.
--
-- >>> twoAdjacent [1,2,3,4]
-- False
-- >>> twoAdjacent [1,2,2,4]
-- True
-- >>> twoAdjacent [1,2,2,3,4,5]
-- True
twoAdjacent :: [Int] -> Bool
twoAdjacent xs = any same $ divvy 2 1 xs
  where
    same [a, b] = a == b

-- | Does the number contain two adjacent matching digits
--   that are no part of a larger group?
--
-- >>> twoAdjacent' $ digits 112233
-- True
-- >>> twoAdjacent' $ digits 123444
-- False
-- >>> twoAdjacent' $ digits 111122
-- True
twoAdjacent' :: [Int] -> Bool
twoAdjacent' = any (\x -> length x == 2) . group

-- |
--
-- >>> sameOrIncreasing [1,1,1,1,2,3]
-- True
-- >>> sameOrIncreasing [1,3,5,6,7,9]
-- True
sameOrIncreasing :: [Int] -> Bool
sameOrIncreasing = all (\[x, y] -> y >= x) . divvy 2 1

-- |
--
-- >>> validPassword twoAdjacent $ digits 111111
-- True
-- >>> validPassword twoAdjacent $ digits 223450
-- False
-- >>> validPassword twoAdjacent $ digits 123789
-- False
validPassword :: ([Int] -> Bool) -> [Int] -> Bool
validPassword adjacer xs = sameOrIncreasing xs && adjacer xs

-- | Main runner.
countValidPasswords ::
  -- | adjacent function
  ([Int] -> Bool) ->
  -- | lower bound of range to search within
  Int ->
  -- | upper bound of range to search within
  Int ->
  Int
countValidPasswords adjacer lo hi = length $ filter (validPassword adjacer . digits) [lo .. hi]
