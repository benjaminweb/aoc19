{-# LANGUAGE OverloadedStrings #-}

module Day6 where

import qualified Data.ByteString.Char8 as BS8 (ByteString, drop, length, lines, pack, readFile, takeWhile)

data Around = Planet BS8.ByteString | UniversalCenterOfMass deriving (Show)

data Relation = Orbit Relation Relation | NoOrbit Around deriving (Show)

type Count = Int

getInput :: FilePath -> IO [BS8.ByteString]
getInput = (BS8.lines <$>) . BS8.readFile

-- |
--
-- >>> parseMap $ BS8.pack "COM)B"
-- Just (Orbit (NoOrbit UniversalCenterOfMass) (NoOrbit (Planet "B")))
--
-- >>> parseMap $ BS8.pack "B)C"
-- Just (Orbit (NoOrbit (Planet "B")) (NoOrbit (Planet "C")))
parseMap :: BS8.ByteString -> Maybe Relation
parseMap xs
  | first == BS8.pack "COM" = Just $ Orbit (NoOrbit UniversalCenterOfMass) (NoOrbit (Planet second))
  | otherwise = Just $ Orbit (NoOrbit (Planet first)) (NoOrbit (Planet second))
  where
    first = BS8.takeWhile (/= ')') xs
    second = BS8.drop (BS8.length first + 1) xs

-- :: Relation -> Mapping -> Maybe Relation


directOrbits :: [Relation] -> Count
directOrbits = undefined

indirectOrbits :: [Relation] -> Count
indirectOrbits = undefined

totalOrbits :: [Relation] -> Count
totalOrbits xs = directOrbits xs + indirectOrbits xs
