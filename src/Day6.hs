{-# LANGUAGE OverloadedStrings #-}

module Day6 where

import Data.Maybe (mapMaybe)

import qualified Data.ByteString.Char8 as BS8 (ByteString, drop, length, lines, pack, readFile, takeWhile)

data Relation = Orbit Relation Relation | Planet BS8.ByteString | UniversalCenterOfMass deriving (Show, Eq)

type Count = Int

getInput :: FilePath -> IO [Relation]
getInput = (mapMaybe parseMap . BS8.lines <$>) . BS8.readFile

-- |
--
-- >>> parseMap $ BS8.pack "COM)B"
-- Just (Orbit UniversalCenterOfMass (Planet "B"))
--
-- >>> parseMap $ BS8.pack "B)C"
-- Just (Orbit (Planet "B") (Planet "C"))
parseMap :: BS8.ByteString -> Maybe Relation
parseMap xs
  | first == BS8.pack "COM" = Just $ Orbit UniversalCenterOfMass (Planet second)
  | otherwise = Just $ Orbit (Planet first) (Planet second)
  where
    first = BS8.takeWhile (/= ')') xs
    second = BS8.drop (BS8.length first + 1) xs

-- | Merge two `Relation`s if one is part of the other.
-- 
-- >>> mergeRelations (Orbit UniversalCenterOfMass (Planet "B")) $ Orbit (Planet "B") (Planet "C")
-- Just (Orbit UniversalCenterOfMass (Orbit (Planet "B") (Planet "C")))
--
-- >>> mergeRelations (Orbit UniversalCenterOfMass (Orbit (Planet "B") (Planet "C"))) $ Orbit (Planet "C") (Planet "D")
-- Just (Orbit UniversalCenterOfMass (Orbit (Planet "B") (Orbit (Planet "C") (Planet "D"))))
--
-- >>> mergeRelations (Orbit UniversalCenterOfMass (Orbit (Planet "B") (Orbit (Planet "C") (Planet "D")))) $ Orbit (Planet "D") (Planet "E")
-- Just (Orbit UniversalCenterOfMass (Orbit (Planet "B")) (Orbit (Planet "C") (Orbit (Planet "D") (Planet "E"))))
mergeRelations :: Relation -> Relation -> Maybe Relation
mergeRelations (Orbit first second) b@(Orbit third fourth)
  | first == third = Just $ Orbit (Orbit first (Orbit third fourth)) second
  | second == third = Just $ Orbit first (Orbit second fourth)
  | otherwise = case mergeRelations second b of
                  Just snd' -> Just $ Orbit first snd'
                  Nothing -> case mergeRelations first b of
                                Just fst' -> Just $ Orbit fst' second
                                Nothing -> Nothing


directOrbits :: [Relation] -> Count
directOrbits = undefined

indirectOrbits :: [Relation] -> Count
indirectOrbits = undefined

totalOrbits :: [Relation] -> Count
totalOrbits xs = directOrbits xs + indirectOrbits xs
