{-# LANGUAGE OverloadedStrings #-}

module Day6 where

import qualified Data.ByteString.Char8 as BS8 (ByteString, drop, length, lines, pack, readFile, takeWhile)

type Planet = BS8.ByteString

-- data Orbit = WhatAroundWhat Planet Orbit | UniversalCenterOfMass Planet deriving Show
newtype Orbit = Orbit ()

data Around = Planet BS8.ByteString | UniversalCenterOfMass deriving (Show)

data Mapping = Mapping (Around, Around) deriving (Show)

type Count = Int

getInput :: FilePath -> IO [BS8.ByteString]
getInput = (BS8.lines <$>) . BS8.readFile

-- |
--
-- >>> parseMap $ BS8.pack "COM)B"
-- Just (Mapping (UniversalCenterOfMass,Planet "B"))
--
-- >>> parseMap $ BS8.pack "B)C"
-- Just (Mapping (Planet "B",Planet "C"))
parseMap :: BS8.ByteString -> Maybe Mapping
parseMap xs
  | first == BS8.pack "COM" = Just $ Mapping (UniversalCenterOfMass, Planet second)
  | otherwise = Just $ Mapping (Planet first, Planet second)
  where
    first = BS8.takeWhile (/= ')') xs
    second = BS8.drop (BS8.length first + 1) xs

directOrbits :: [Orbit] -> Count
directOrbits = undefined

indirectOrbits :: [Orbit] -> Count
indirectOrbits = undefined

totalOrbits :: [Orbit] -> Count
totalOrbits xs = directOrbits xs + indirectOrbits xs
