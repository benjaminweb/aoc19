module Day8 where

import Data.List.Split (divvy)
import Data.List (sortOn)
import Data.Maybe (mapMaybe)
import Text.Read (readMaybe)

type Colour = Int

type Row = [Colour]

type Layer = [Row]

type Picture = [Layer]

type Width = Int

type Height = Int

type Count = Int

type Checksum = Int

getInput :: FilePath -> IO [Int]
getInput = (strToInts <$>) . readFile

-- >>> strToInts "123456789012"
-- [1,2,3,4,5,6,7,8,9,0,1,2]
strToInts :: String -> [Int]
strToInts = mapMaybe (\x -> (readMaybe [x] :: Maybe Int))

-- >>> parsePicture 3 2 [1,2,3,4,5,6,7,8,9,0,1,2]
-- [[[1,2,3],[4,5,6]],[[7,8,9],[0,1,2]]]
parsePicture :: Width -> Height -> [Int] -> Picture
parsePicture w h = map (divvy w w) . divvy layerCount layerCount
  where layerCount = w * h

-- >>> countVal 1 [[1,2,3],[4,5,6]]
-- 1
countVal :: Int -> Layer -> Count
countVal x rows = length $ filter (== x) $ concat rows

-- >>> countPerLayer [[[1,2,3],[4,5,6]],[[7,8,9],[0,1,2]]] 1
-- [(0,1),(1,1)]
countPerLayer :: Picture -> Int -> [(Int, Count)]
countPerLayer layers x = zip [0..] $ map (countVal x) layers

checksum :: Picture -> Checksum
checksum pic = digits1 * digits2
  where ((layerNo, _):_) = sortOn snd $ countPerLayer pic 0
        digits1 = countVal 1 $ pic !! layerNo
        digits2 = countVal 2 $ pic !! layerNo
