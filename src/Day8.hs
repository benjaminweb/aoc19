module Day8 where

import Data.List (sortOn, transpose)
import Data.List.Split (divvy)
import Data.Maybe (listToMaybe, mapMaybe)
import Text.Read (readMaybe)

type ColourCode = Int

data Colour = Black | White | Transparent deriving (Eq)

instance Show Colour where
  show Black = " "
  show White = "▊"
  show Transparent = " "

type Row a = [a]

type Layer a = [Row a]

type Picture a = [Layer a]

type EffectivePicture a = Layer a

type Width = Int

type Height = Int

type Count = Int

type Checksum = Int

getInput :: FilePath -> IO [ColourCode]
getInput = (strToInts <$>) . readFile

-- >>> strToInts "123456789012"
-- [1,2,3,4,5,6,7,8,9,0,1,2]
strToInts :: String -> [Int]
strToInts = mapMaybe (\x -> readMaybe [x] :: Maybe Int)

-- >>> parsePicture 3 2 [1,2,3,4,5,6,7,8,9,0,1,2]
-- [[[1,2,3],[4,5,6]],[[7,8,9],[0,1,2]]]
parsePicture :: Width -> Height -> [Int] -> Picture ColourCode
parsePicture w h = map (divvy w w) . divvy layerCount layerCount
  where
    layerCount = w * h

-- >>> countVal 1 [[1,2,3],[4,5,6]]
-- 1
countVal :: Int -> Layer ColourCode -> Count
countVal x rows = length $ filter (== x) $ concat rows

-- >>> countPerLayer [[[1,2,3],[4,5,6]],[[7,8,9],[0,1,2]]] 1
-- [(0,1),(1,1)]
countPerLayer :: Picture ColourCode -> Int -> [(Int, Count)]
countPerLayer layers x = zip [0 ..] $ map (countVal x) layers

checksum :: Picture ColourCode -> Checksum
checksum pic = digits1 * digits2
  where
    ((layerNo, _) : _) = sortOn snd $ countPerLayer pic 0
    digits1 = countVal 1 $ pic !! layerNo
    digits2 = countVal 2 $ pic !! layerNo

toColour :: ColourCode -> Colour
toColour 0 = Black
toColour 1 = White
toColour 2 = Transparent

-- >>> effectiveColour [Transparent,Black,White]
-- Just Black
--
-- >>> effectiveColour [Transparent,Transparent,White]
-- Just White
--
-- >>> effectiveColour [Black,Transparent,White]
-- Just Black
--
-- >>> effectiveColour [Transparent,Transparent,Transparent]
effectiveColour :: [Colour] -> Maybe Colour
effectiveColour [] = Nothing
effectiveColour xs = listToMaybe $ dropWhile (== Transparent) $ init xs ++ [last xs]

toColours :: Picture ColourCode -> Picture Colour
toColours = map $ map $ map toColour

foldLayers :: Picture Colour -> EffectivePicture Colour
foldLayers = map foldRows . transpose

-- >>> foldRows [[Black, Transparent], [White, Transparent]]
-- [Black,Transparent]
foldRows :: [Row Colour] -> Row Colour
foldRows = mapMaybe effectiveColour . transpose

printPic :: EffectivePicture Colour -> IO [()]
printPic = mapM print
