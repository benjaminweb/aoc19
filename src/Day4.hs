module Day4 where

import Data.List.Split

-- | Dissect Int into list of its digits.
--
-- >>> digits 120
-- [1,2,0]
-- >>> digits 1
-- [1]
-- >>> digits 12
-- [1,2]
digits :: Int -> [Int]
digits z = reverse $ fst $ last $ take toTake $ iterate (\(x, y) -> (x ++ [mod y 10], div y 10)) ([], z)
  where
    noDigits x
      | x < 10 = 1
      | otherwise = ceiling $ logBase 10 x
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
-- >>> validPassword $ digits 111111
-- True
-- >>> validPassword $ digits 223450
-- False
-- >>> validPassword $ digits 123789
-- False
validPassword :: [Int] -> Bool
validPassword xs = sameOrIncreasing xs && twoAdjacent xs

-- | Main runner.
countValidPasswords ::
  -- | lower bound of range to search within
  Int ->
  -- | upper bound of range to search within
  Int ->
  Int
countValidPasswords lo hi = length $ filter (validPassword . digits) [lo .. hi]
