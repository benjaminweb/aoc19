module Day1 where

import qualified Data.ByteString.Char8 as BS8 (lines, readFile, readInt)
import Data.Maybe (mapMaybe)

type Mass = Int

type Fuel = Int

getInput :: FilePath -> IO [Mass]
getInput path = (map fst . mapMaybe BS8.readInt . BS8.lines) <$> BS8.readFile path

-- | Calculates the fuel of a module given its mass.
--
-- >>> startFuelRequired 12
-- 2
-- >>> startFuelRequired 14
-- 2
-- >>> startFuelRequired 1969
-- 654
-- >>> startFuelRequired 100756
-- 33583
startFuelRequired :: Mass -> Fuel
startFuelRequired = (+ (-2)) . (`div` 3)

-- | Calculates the sum of fuel requirements for all modules given their mass.
totalFuelRequirement :: [Mass] -> Fuel
totalFuelRequirement = sum . map (uncurry (+) .(\x -> (startFuelRequired x, fuelForFuelForMass x)))

-- | Determine for a given mass required fuel for fuel.
--
-- >>> fuelForFuelForMass 14
-- 0
-- >>> fuelForFuelForMass 1969
-- 312
-- >>> fuelForFuelForMass 100756
-- 16763
fuelForFuelForMass :: Mass -> Fuel
fuelForFuelForMass mass = go firstAdditionalFuel []
  where
    go x acc
      | x <= 0 = sum acc
      | otherwise = go (startFuelRequired x) $ acc ++ [x]
    firstAdditionalFuel = startFuelRequired $ startFuelRequired mass
