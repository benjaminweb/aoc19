{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE RecordWildCards #-}

module Day2 where

import qualified Data.ByteString.Char8 as B8
import Data.Foldable (foldl')
import Data.List.Split (divvy, splitOn)
import Data.Maybe (fromMaybe, listToMaybe, mapMaybe)
import qualified Data.Sequence as Seq
import Data.Sequence ((<|), (><), Seq ((:<|), (:|>), Empty), (|>))
import Optics.At.Core (ix)
import Optics.Operators ((.~))
import Optics.Optic ((&))

getInput :: FilePath -> IO (Seq Int)
getInput = (Seq.fromList . map fst . mapMaybe B8.readInt . B8.split ',' <$>) . B8.readFile

data Operation = Add | Mul deriving (Show, Eq)

data Instruction
  = Instruction
      { iOp :: Operation,
        iSrcPos1 :: Int,
        iSrcPos2 :: Int,
        iStoragePos :: Int
      }
  | Terminate
  | Error (Seq Int)
  deriving (Show, Eq)

isError :: Instruction -> Bool
isError (Error _) = True
isError _ = False

-- |
--
-- >>> toInstruction $ Seq.fromList [1,9,10,3]
-- Instruction {iOp = Add, iSrcPos1 = 9, iSrcPos2 = 10, iStoragePos = 3}
-- >>> toInstruction $ Seq.fromList [2,9,10,3]
-- Instruction {iOp = Mul, iSrcPos1 = 9, iSrcPos2 = 10, iStoragePos = 3}
-- >>> toInstruction $ Seq.fromList [99]
-- Terminate
-- >>> toInstruction $ Seq.fromList [100]
-- Error (fromList [100])
toInstruction :: Seq Int -> Instruction
toInstruction (1 :<| iSrcPos1 :<| iSrcPos2 :<| iStoragePos :<| Empty) = let iOp = Add in Instruction {..}
toInstruction (2 :<| iSrcPos1 :<| iSrcPos2 :<| iStoragePos :<| Empty) = let iOp = Mul in Instruction {..}
toInstruction (99 :<| Empty) = Terminate
toInstruction xs = Error xs

parse :: (Seq (Seq Int), Seq Int) -> (Seq (Seq Int), Seq Int)
parse (done, Empty) = (done, Empty)
parse (done, todo@(x :<| _)) = parse (done |> moving, remaining)
  where
    parametricity x = case x of
      1 -> 1 + 3
      2 -> 1 + 3
      99 -> 1
      _ -> 1
    moving = Seq.take (parametricity x) todo
    remaining = Seq.drop (parametricity x) todo

execInstruction :: (Int, Seq Int) -> Instruction -> (Int, Seq Int)
execInstruction (c, xs) Instruction {..} = case iOp of
  Add -> (c + 1 + 3, xs & ix iStoragePos .~ uncurry (+) getPosPair)
  Mul -> (c + 1 + 3, xs & ix iStoragePos .~ uncurry (*) getPosPair)
  where
    getPos x = fromMaybe 0 $ Seq.lookup x xs
    getPosPair = (getPos iSrcPos1, getPos iSrcPos2)

-- | Core logic.
--
-- >>> umbrella (0, Seq.fromList [1,0,0,0,99])
-- (4,fromList [2,0,0,0,99])
-- >>> umbrella (0, Seq.fromList [2,3,0,3,99])
-- (4,fromList [2,3,0,6,99])
-- >>> umbrella (0, Seq.fromList [2,4,4,5,99,0])
-- (4,fromList [2,4,4,5,99,9801])
-- >>> umbrella (0, Seq.fromList [1,1,1,4,99,5,6,0,99])
-- (8,fromList [30,1,1,4,2,5,6,0,99])
umbrella :: (Int, Seq Int) -> (Int, Seq Int)
umbrella (c, xs) =
  case nextInstruction (Seq.drop c xs) of
    Just istr@Instruction {} -> umbrella $ execInstruction (c, xs) istr
    Just Terminate -> (c, xs)
    Just (Error (x :<| Empty)) -> error $ "invalid opCode " ++ show x
    Just (Error xs) -> error $ "invalid opCode " ++ show xs

nextInstruction :: Seq Int -> Maybe Instruction
nextInstruction = seqToMaybe . fmap toInstruction . fst . parse . (,) Empty

seqToMaybe :: Seq a -> Maybe a
seqToMaybe Empty = Nothing
seqToMaybe (x :<| _) = Just x

prepare :: Int -> Int -> Seq Int -> Seq Int
prepare first second xs = xs & ix 1 .~ first & ix 2 .~ second

intCode :: (Seq Int -> Seq Int) -> Seq Int -> Int
intCode preparer = fromMaybe 0 . Seq.lookup 0 . snd . umbrella . (,) 0 . preparer

findPair :: Seq Int -> Int -> Maybe (Int, Int)
findPair is z = seqToMaybe $ Seq.filter (\(x, y) -> intCode (prepare x y) is == z) $ Seq.fromList [(x, y) | x <- [0 .. 99], y <- [0 .. 99]]

formatPair :: Int -> Int -> Int
formatPair noun verb = 100 * noun + verb
