{-# LANGUAGE OverloadedStrings #-}

module Day6 where

import Data.List.Split (splitOn)
import qualified Data.Map.Strict as Map

type Count = Int

getInput :: FilePath -> IO (Map.Map String String)
getInput = fmap (Map.fromList . map ((\[x, y] -> (y, x)) . splitOn ")") . lines) . readFile

-- |
--
-- >>> countOrbits 0 "A" $ Map.fromList [("A", "COM")]
-- 1
--
-- >>> countOrbits 0 "B" $ Map.fromList [("A", "COM"), ("B", "A")]
-- 2
countOrbits :: Int -> String -> Map.Map String String -> Count
countOrbits c k m = case Map.lookup k m of
  Just "COM" -> c + 1
  Just x -> countOrbits (c + 1) x m
  Nothing -> error $ "key " ++ "k not found!"

-- |
--
-- >>> countTotalOrbits  $ Map.fromList [("A", "COM"), ("B", "A")]
-- 3
--
-- >>> countTotalOrbits  $ Map.fromList [("A", "COM"), ("B", "A"), ("C", "B")]
-- 6
countTotalOrbits :: Map.Map String String -> Count
countTotalOrbits m = sum $ map (\x -> countOrbits 0 x m) $ Map.keys m
