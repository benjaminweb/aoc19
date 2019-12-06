{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}

module Day5 where

import qualified Data.ByteString.Char8 as B8
import Data.Foldable (foldl')
import Data.List.Split (divvy, splitOn)
import Data.Maybe (fromMaybe, listToMaybe, mapMaybe)
import qualified Data.Sequence as Seq
import Data.Sequence ((<|), (><), Seq ((:<|), (:|>), Empty), (|>))
import Day4 (digits)
import Optics.At.Core (ix)
import Optics.Operators ((.~))
import Optics.Optic ((&))

getInput :: FilePath -> IO (Seq Int)
getInput = (Seq.fromList . map fst . mapMaybe B8.readInt . B8.split ',' <$>) . B8.readFile

data Operation = Add | Mul | Save | Print deriving (Show, Eq)

data Instruction
  = Instruction
      { iOp :: Operation,
        iSrcPara1 :: (Mode, Int),
        iSrcPara2 :: (Mode, Int),
        iStoragePos :: Int
      }
  | NewInstruction -- Save, Print
      { niOp :: Operation,
        niPara :: (Mode, Int)
      }
  | Terminate
  | Error (Seq Int)
  deriving (Show, Eq)

data Mode = Position | Immediate deriving (Show, Eq)

isError :: Instruction -> Bool
isError (Error _) = True
isError _ = False

-- |
--
-- >>> toInstruction $ Seq.fromList [1002,4,3,4]
-- Instruction {iOp = Mul, Pos1 = 4, iSrcPos2 = 3, iStoragePos = 4}
--
-- >>> toInstruction $ Seq.fromList [1,9,10,3]
-- Instruction {iOp = Add, Pos1 = 9, iSrcPos2 = 10, iStoragePos = 3}
-- >>> toInstruction $ Seq.fromList [2,9,10,3]
-- Instruction {iOp = Mul, Pos1 = 9, iSrcPos2 = 10, iStoragePos = 3}
-- >>> toInstruction $ Seq.fromList [99]
-- Terminate
-- >>> toInstruction $ Seq.fromList [100]
-- Error (fromList [100])
toInstruction :: Seq Int -> Instruction
toInstruction (extOpCode :<| iSrcPara1B :<| iSrcPara2B :<| iStoragePos :<| Empty) = Instruction {..}
  where
    (iOp, pModes) = parseExtOpCode extOpCode
    iSrcPara1
      | null pModes = (Position, iSrcPara1B)
      | otherwise = (pModes !! 0, iSrcPara1B)
    iSrcPara2
      | null pModes = (Position, iSrcPara1B)
      | otherwise = (pModes !! 1, iSrcPara2B)
toInstruction (extOpCode :<| niParaB :<| Empty) = NewInstruction {..}
  where
    (niOp, pModes) = parseExtOpCode extOpCode
    niPara
      | null pModes = (Position, niParaB)
      | otherwise = (pModes !! 0, niParaB)
toInstruction (99 :<| Empty) = Terminate
toInstruction xs = Error xs

toOperation :: OpCode -> Operation
toOperation 1 = Add
toOperation 2 = Mul
toOperation 3 = Save
toOperation 4 = Print
toOperation xs = error $ "invalid opCode " ++ show xs

parse :: (Seq (Seq Int), Seq Int) -> (Seq (Seq Int), Seq Int)
parse (done, Empty) = (done, Empty)
parse (done, todo@(x :<| _)) = parse (done |> moving, remaining)
  where
    parametricity x = case getOpCode x of
      1 -> 1 + 3
      2 -> 1 + 3
      3 -> 1 + 1
      4 -> 1 + 1
      99 -> 1
      _ -> 1
    moving = Seq.take (parametricity x) todo
    remaining = Seq.drop (parametricity x) todo

getPos :: Seq Int -> (Mode, Int) -> Int
getPos xs (Position, x) = fromMaybe 0 $ Seq.lookup x xs
getPos _ (Immediate, x) = x

getPosPair :: Seq Int -> (Mode, Int) -> (Mode, Int) -> (Int, Int)
getPosPair xs para1 para2 = (getPos xs para1, getPos xs para2)

-- |
--
-- >>> execInstruction (0, [], Seq.fromList [1002,4,3,4,33]) Instruction {iOp = Mul, iSrcPara1 = (Position,4), iSrcPara2 = (Immediate,3), iStoragePos = 4}
-- (4,[],fromList [1002,4,3,4,99])
-- >>> execInstruction (0, [], Seq.fromList [1002,4,3,4,33]) NewInstruction {niOp = Print, niPara = (Position,4)}
-- (2,[33],fromList [1002,4,3,4,33])
execInstruction :: (Int, [Int], Seq Int) -> Instruction -> (Int, [Int], Seq Int)
execInstruction (c, prints, xs) Instruction {..} = case iOp of
  Add -> (c + 1 + 3, prints, xs & ix iStoragePos .~ uncurry (+) (getPosPair xs iSrcPara1 iSrcPara2))
  Mul -> (c + 1 + 3, prints, xs & ix iStoragePos .~ uncurry (*) (getPosPair xs iSrcPara1 iSrcPara2))
execInstruction (c, prints, xs) NewInstruction {..} = case niOp of
  Save -> (c + 1 + 1, prints, xs & ix (snd niPara) .~ 1)
  Print -> (c + 1 + 1, prints ++ [getPos xs niPara], xs)

-- | Core logic.
--
-- >>> umbrella (0, Seq.fromList [1,0,0,0,99])
-- (4,[],fromList [2,0,0,0,99])
-- >>> umbrella (0, Seq.fromList [2,3,0,3,99])
-- (4,[],fromList [2,3,0,6,99])
-- >>> umbrella (0, Seq.fromList [2,4,4,5,99,0])
-- (4,[],fromList [2,4,4,5,99,9801])
-- >>> umbrella (0, Seq.fromList [1,1,1,4,99,5,6,0,99])
-- (8,[],fromList [30,1,1,4,2,5,6,0,99])
-- >>> umbrella (0, Seq.fromList [1101,100,-1,4,0])
-- (4,[],fromList [1101,100,-1,4,99])
umbrella :: (Int, [Int], Seq Int) -> (Int, [Int], Seq Int)
umbrella (c, prints, xs) =
  case nextInstruction (Seq.drop c xs) of
    Just istr@Instruction {} -> umbrella $ execInstruction (c, prints, xs) istr
    Just istr@NewInstruction {} -> umbrella $ execInstruction (c, prints, xs) istr
    Just Terminate -> (c, prints, xs)
    Just (Error (x :<| Empty)) -> error $ "invalid opCode " ++ show x
    Just (Error xs) -> error $ "invalid opCode " ++ show xs
    Nothing -> (c, prints, xs)

nextInstruction :: Seq Int -> Maybe Instruction
nextInstruction = seqToMaybe . fmap toInstruction . fst . parse . (,) Empty

seqToMaybe :: Seq a -> Maybe a
seqToMaybe Empty = Nothing
seqToMaybe (x :<| _) = Just x

prepare :: Int -> Int -> Seq Int -> Seq Int
prepare first second xs = xs & ix 1 .~ first & ix 2 .~ second

intCode :: (Seq Int -> Seq Int) -> Seq Int -> Int
intCode preparer = fromMaybe 0 . Seq.lookup 0 . trd' . umbrella . (,,) 0 [] . preparer
  where
    trd' (_, _, x) = x

findPair :: Seq Int -> Int -> Maybe (Int, Int)
findPair is z = seqToMaybe $ Seq.filter (\(x, y) -> intCode (prepare x y) is == z) $ Seq.fromList [(x, y) | x <- [0 .. 99], y <- [0 .. 99]]

formatPair :: Int -> Int -> Int
formatPair noun verb = 100 * noun + verb

-- Day 5.1

type ExtOpCode = Int

type OpCode = Int

-- |
--
-- >>> getOpCode 1002
-- 2
-- >>> getOpCode 11012
-- 2
-- >>> getOpCode 99
-- 99
getOpCode :: ExtOpCode -> OpCode
getOpCode 99 = 99
getOpCode x = (x `mod` 10)

-- |
--
-- >>> modes 11012
-- [1,1,0]
-- >>> modes 1002
-- [0,1,0]
modes :: Int -> [Int]
modes xs = p3 ++ (digits $ div (xs - getOpCode xs) 100)
  where
    p3
      | xs < 10000 = [0]
      | otherwise = []

toMode :: ExtOpCode -> Mode
toMode 0 = Position
toMode 1 = Immediate
toMode x = error $ "invalid mode specified by " ++ show x

-- |
-- >>> parseExtOpCode 1002
-- (Mul,[Position,Immediate,Position])
-- >>> parseExtOpCode 11002
-- (Mul,[Position,Immediate,Immediate])
-- >>> parseExtOpCode 1
-- (Add,[])
-- >>> parseExtOpCode 3
-- (Save,[])
-- >>> parseExtOpCode 104
-- (Print,[Immediate])
parseExtOpCode :: ExtOpCode -> (Operation, [Mode])
parseExtOpCode xs
  | xs < 100 = (toOperation $ getOpCode xs, [])
  | otherwise = (toOperation $ getOpCode xs, reverse . map toMode . modes $ xs)
{-- diagProg ::
diagProg
  where theInput = 1
--}
