{-# LANGUAGE RecordWildCards #-}

module Day2 where

import qualified Data.ByteString.Char8 as B8
import Data.Foldable (foldl')
import Data.List.Split (divvy, splitOn)
import Data.Maybe (mapMaybe)

getInput :: FilePath -> IO [Int]
getInput = (map fst . mapMaybe B8.readInt . B8.split ',' <$>) . B8.readFile

data OpCode = Add | Multiply | Terminate | NoOp deriving (Show, Eq)

isNoOp :: Operation -> Bool
isNoOp Operation {..} = opCode == NoOp

data Operation
  = Operation
      { opCode :: OpCode,
        opSrcPos1 :: Int,
        opSrcPos2 :: Int,
        opStoragePos :: Int
      }
  deriving (Show)

-- | Convert four integers to Just Operator, Nothing otherwise.
--
-- >>> Just (Operation {opCode = Add, opSrcPos1 = 9, opSrcPos2 = 10, opStoragePos = 3})
-- Just (Operation {opCode = Add, opSrcPos1 = 9, opSrcPos2 = 10, opStoragePos = 3})
toOperation :: [Int] -> Operation
toOperation [a, opSrcPos1, opSrcPos2, opStoragePos] = Operation {..}
  where
    opCode = case a of
      1 -> Add
      2 -> Multiply
      99 -> Terminate
      _ -> NoOp

-- | Overwrite pos in acc with value x.
--
-- >>> saveResult [1,2,3] 0 99
-- [99,2,3]
-- >>> saveResult [1,2,3] 1 99
-- [1,99,3]
saveResult :: [Int] -> Int -> Int -> [Int]
saveResult acc pos x = take pos acc ++ [x] ++ drop (pos + 1) acc

applyOperation :: [Int] -> Operation -> [Int]
applyOperation acc Operation {..} = case opCode of
  Add -> saveResult acc opStoragePos $ uncurry (+) getPosPair
  Multiply -> saveResult acc opStoragePos $ uncurry (*) getPosPair
  Terminate -> acc
  NoOp -> acc
  where
    getPos x = acc !! max 0 x
    getPosPair = (getPos opSrcPos1, getPos opSrcPos2)

execOp :: (Int, [Int]) -> (Int, [Int])
execOp (c, xs) = let op = getOperation c xs in (c + 1, applyOperation xs op)

getOperation :: Int -> [Int] -> Operation
getOperation n xs = toOperation (divvy 4 4 xs !! max 0 n)

-- FIXME: stop at termination

-- | Core logic.
--
-- >>> go [1,0,0,0,99]
-- [2,0,0,0,99]
-- >>> go [2,3,0,3,99]
-- [2,3,0,6,99]
-- >>> go [2,4,4,5,99,0]
-- [2,4,4,5,99,9801]
-- >>> go [1,1,1,4,99,5,6,0,99]
-- [30,1,1,4,2,5,6,0,99]
go :: [Int] -> [Int]
go xs = snd . last . take (limit + 1) . iterate execOp . (,) 0
  where
    limit = div (length xs) 4

prepare :: [Int] -> [Int]
prepare xs = saveResult (saveResult xs 1 12) 2 2

intCode :: [Int] -> Int
intCode = head . go . prepare
