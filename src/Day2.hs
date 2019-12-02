{-# LANGUAGE RecordWildCards #-}

-- FIXME: error handling, if invalid OpCode

module Day2 where

import qualified Data.ByteString.Char8 as B8
import Data.Foldable (foldl')
import Data.List.Split (divvy, splitOn)
import Data.Maybe (listToMaybe, mapMaybe)

getInput :: FilePath -> IO [Int]
getInput = (map fst . mapMaybe B8.readInt . B8.split ',' <$>) . B8.readFile

data Operation = Add | Mul deriving (Show, Eq)

data Instruction
  = Instruction
      { iOp :: Operation,
        iSrcPos1 :: Int,
        iSrcPos2 :: Int,
        iStoragePos :: Int
      }
  | Terminate
  | Error [Int]
  deriving (Show, Eq)

isError :: Instruction -> Bool
isError (Error _) = True
isError _ = False

-- >>> toInstruction [1,9,10,3]
-- Instruction {iOp = Add, iSrcPos1 = 9, iSrcPos2 = 10, iStoragePos = 3})
-- >>> toInstruction [2,9,10,3]
-- Instruction {iOp = Mul, iSrcPos1 = 9, iSrcPos2 = 10, iStoragePos = 3}
-- >>> toInstruction [99]
-- Terminate
-- >>> toInstruction [100]
-- Error [100]
toInstruction :: [Int] -> Instruction
toInstruction [1, iSrcPos1, iSrcPos2, iStoragePos] = let iOp = Add in Instruction {..}
toInstruction [2, iSrcPos1, iSrcPos2, iStoragePos] = let iOp = Mul in Instruction {..}
toInstruction [99] = Terminate
toInstruction xs = Error xs

parse :: ([[Int]], [Int]) -> ([[Int]], [Int])
parse (done, []) = (done, [])
parse (done, todo@(x : _)) = parse (done ++ [moving], remaining)
  where
    parametricity x = case x of
      1 -> 1 + 3
      2 -> 1 + 3
      99 -> 1
      _ -> 1
    moving = take (parametricity x) todo
    remaining = drop (parametricity x) todo

-- | Convert four integers to Just Operator, Nothing otherwise.
--
-- | Overwrite pos in acc with value x.
--
-- >>> saveResult [1,2,3] 0 99
-- [99,2,3]
-- >>> saveResult [1,2,3] 1 99
-- [1,99,3]
saveResult :: [Int] -> Int -> Int -> [Int]
saveResult acc pos x = take pos acc ++ [x] ++ drop (pos + 1) acc

execInstruction :: (Int, [Int]) -> Instruction -> (Int, [Int])
execInstruction (c, xs) Instruction {..} = case iOp of
  Add -> (c + 1 + 3, saveResult xs iStoragePos $ uncurry (+) getPosPair)
  Mul -> (c + 1 + 3, saveResult xs iStoragePos $ uncurry (*) getPosPair)
  where
    getPos x = xs !! max 0 x
    getPosPair = (getPos iSrcPos1, getPos iSrcPos2)

-- | Core logic.
--
-- >>> umbrella (0,[1,0,0,0,99])
-- (4,[2,0,0,0,99])
-- >>> umbrella (0, [2,3,0,3,99])
-- (4,[2,3,0,6,99])
-- >>> umbrella (0, [2,4,4,5,99,0])
-- (4,[2,4,4,5,99,9801])
-- >>> umbrella (0, [1,1,1,4,99,5,6,0,99])
-- (8,[30,1,1,4,2,5,6,0,99])
umbrella :: (Int, [Int]) -> (Int, [Int])
umbrella (c, xs) =
  case nextInstruction (drop c xs) of
    Just istr@(Instruction {}) -> umbrella $ execInstruction (c, xs) istr
    Just Terminate -> (c, xs)

nextInstruction :: [Int] -> Maybe Instruction
nextInstruction = listToMaybe . map toInstruction . fst . parse . (,) []

prepare :: [Int] -> [Int]
prepare xs = saveResult (saveResult xs 1 12) 2 2

intCode :: [Int] -> Int
intCode = head . snd . umbrella . (,) 0 . prepare
