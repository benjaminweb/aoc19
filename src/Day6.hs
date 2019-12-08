{-# LANGUAGE OverloadedStrings #-}

module Day6 where

import Data.List.Split (splitOn)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

type Count = Int

getInput :: FilePath -> IO (Map.Map String String)
getInput = fmap (Map.fromList . map ((\[x, y] -> (y, x)) . splitOn ")") . lines) . readFile

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

-- >>> countTotalOrbits  $ Map.fromList [("A", "COM"), ("B", "A")]
-- 3
--
-- >>> countTotalOrbits  $ Map.fromList [("A", "COM"), ("B", "A"), ("C", "B")]
-- 6
countTotalOrbits :: Map.Map String String -> Count
countTotalOrbits m = sum $ map (\x -> countOrbits 0 x m) $ Map.keys m

-- | Get list of ancestors for given object.
--
-- >>> getAncestors (Map.fromList [("A", "COM"), ("B", "A"), ("C", "B")]) "C" []
-- ["B","A","COM"]
getAncestors :: Map.Map String String -> String -> [String] -> [String]
getAncestors m k acc = case Map.lookup k m of
  Just "COM" -> acc ++ ["COM"]
  Just x -> getAncestors m x $ acc ++ [x]
  Nothing -> error $ "key " ++ k ++ " not present in map!"

-- >>> getEffectiveOrbits ["Z", "B","A","COM"] ["G", "F", "B", "A", "COM"]
-- (["Z","B"], ["G","F","B"])
getEffectiveOrbits :: [String] -> [String] -> ([String], [String])
getEffectiveOrbits a b = (a' ++ [rendezvous], b' ++ [rendezvous])
  where
    b' = takeWhile (\x -> not $ x `elem` a) b
    a' = takeWhile (\x -> not $ x `elem` b) a
    rendezvous = head $ drop (length a') a

-- >>> minimumTransfers ["D"] ["K", "J", "E", "D"]
-- 3
--
-- >>> minimumTransfers ["G", "C"] ["K", "J", "E", "D", "C"]
-- 5
minimumTransfers :: [String] -> [String] -> Count
minimumTransfers [] _ = error "list length must be > 0"
minimumTransfers _ [] = error "list length must be > 0"
minimumTransfers a b = (+ (-1)) $ Set.size $ Set.fromList $ a ++ b

-- >>> detMinimumTransfers "SAN" "YOU" (Map.fromList [("YOU", "K"), ("K", "J"), ("J", "E"), ("E", "D"), ("I", "D"), ("SAN", "I"), ("D", "C"), ("C", "B"), ("B", "A"), ("A", "COM")])
-- 4
detMinimumTransfers :: String -> String -> Map.Map String String -> Count
detMinimumTransfers a b m = uncurry minimumTransfers $ getEffectiveOrbits (getAncestors m a []) (getAncestors m b [])
