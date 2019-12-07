module IntCode where

import Data.Bool (bool)
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

data Instruction = Add (Maybe ThreeParameter) | Mul (Maybe ThreeParameter) | Save (Maybe (Val, Pos)) | Print (Maybe Parameter) | JumpIfTrue (Maybe TwoParameter) | JumpIfFalse (Maybe TwoParameter) | LessThan (Maybe ThreeParameter) | Equals (Maybe ThreeParameter) | Terminate | Error (Seq Int) deriving (Show, Eq)

type ValOrPos = Int

type Pos = Int

type Val = Int

data Mode = Position | Immediate deriving (Show, Eq)

type Parameter = (Mode, ValOrPos)

type TwoParameter = (Parameter, Parameter)

type ThreeParameter = (Parameter, Parameter, Pos)

type ExtOpCode = Int

type OpCode = Int

isError :: Instruction -> Bool
isError (Error _) = True
isError _ = False

getInput :: FilePath -> IO (Seq Int)
getInput = (Seq.fromList . map fst . mapMaybe B8.readInt . B8.split ',' <$>) . B8.readFile

-- |
--
-- >>> execInstruction (0, [], Seq.fromList [1,2,4,6,3,0,0]) $ Add (Just ((Position,2),(Position,4),6))
-- (4,[],fromList [1,2,4,6,3,0,7])
--
-- >>> execInstruction (0, [], Seq.fromList [101,2,3,4,0,0]) $ Add (Just ((Immediate,2),(Position,3),4))
-- (4,[],fromList [101,2,3,4,6,0])
--
-- >>> execInstruction (0, [], Seq.fromList [1001,2,1,4,0,0]) $ Add (Just ((Position,2),(Immediate,1),4))
-- (4,[],fromList [1001,2,1,4,2,0])
--
-- >>> execInstruction (0, [], Seq.fromList [1101,3,1,4,0,0]) $ Add (Just ((Immediate,3),(Immediate,1),4))
-- (4,[],fromList [1101,3,1,4,4,0])
--
-- >>> execInstruction (0, [], Seq.fromList [2,2,4,6,3,0,0]) $ Mul (Just ((Position,2),(Position,4),6))
-- (4,[],fromList [2,2,4,6,3,0,12])
--
-- >>> execInstruction (0, [], Seq.fromList [102,2,4,6,3,0,0]) $ Mul (Just ((Immediate,2),(Position,4),6))
-- (4,[],fromList [102,2,4,6,3,0,6])
--
-- >>> execInstruction (0, [], Seq.fromList [1102,5,4,6,3,0,0]) $ Mul (Just ((Immediate,5),(Immediate,4),6))
-- (4,[],fromList [1102,5,4,6,3,0,20])
--
-- >>> execInstruction (0, [], Seq.fromList [3,2,0]) $ Save (Just (2,11))
-- (2,[],fromList [3,2,11])
--
-- >>> execInstruction (0, [], Seq.fromList [4,2,0]) $ Print (Just (Position,2))
-- (2,[0],fromList [4,2,0])
--
-- >>> execInstruction (0, [], Seq.fromList [5,0,0]) $ JumpIfTrue (Just ((Position,0),(Position,0)))
-- (5,[],fromList [5,0,0])
--
-- >>> execInstruction (0, [], Seq.fromList [5,1,0]) $ JumpIfTrue (Just ((Position,1),(Position,0)))
-- (5,[],fromList [5,1,0])
--
-- >>> execInstruction (0, [], Seq.fromList [105,1,0]) $ JumpIfTrue (Just ((Immediate,0),(Position,99)))
-- (3,[],fromList [105,1,0])
--
-- >>> execInstruction (0, [], Seq.fromList [1005,1,97]) $ JumpIfTrue (Just ((Position,1),(Immediate,97)))
-- (97,[],fromList [1005,1,97])
--
-- >>> execInstruction (0, [], Seq.fromList [1105,1,97]) $ JumpIfTrue (Just ((Immediate,1),(Immediate,97)))
-- (97,[],fromList [1105,1,97])
--
-- >>> execInstruction (0, [], Seq.fromList [1105,0,97]) $ JumpIfTrue (Just ((Immediate,0),(Immediate,97)))
-- (3,[],fromList [1105,0,97])
--
-- >>> execInstruction (0, [], Seq.fromList [6,0,0]) $ JumpIfFalse (Just ((Position,0),(Position,0)))
-- (3,[],fromList [6,0,0])
--
-- >>> execInstruction (0, [], Seq.fromList [6,1,0]) $ JumpIfFalse (Just ((Position,1),(Position,0)))
-- (3,[],fromList [6,1,0])
--
-- >>> execInstruction (0, [], Seq.fromList [106,1,0]) $ JumpIfFalse (Just ((Immediate,0),(Position,99)))
-- (0,[],fromList [106,1,0])
--
-- >>> execInstruction (0, [], Seq.fromList [1006,1,97]) $ JumpIfFalse (Just ((Position,1),(Immediate,97)))
-- (3,[],fromList [1006,1,97])
--
-- >>> execInstruction (0, [], Seq.fromList [1106,1,97]) $ JumpIfFalse (Just ((Immediate,1),(Immediate,97)))
-- (3,[],fromList [1106,1,97])
--
-- >>> execInstruction (0, [], Seq.fromList [1106,0,97]) $ JumpIfFalse (Just ((Immediate,0),(Immediate,97)))
-- (97,[],fromList [1106,0,97])
--
-- >>> execInstruction (0, [], Seq.fromList [7,0,1,4,0]) $ LessThan (Just ((Position,0),(Position,1),4))
-- (4,[],fromList [7,0,1,4,0])
--
-- >>> execInstruction (0, [], Seq.fromList [107,0,1,4,0]) $ LessThan (Just ((Immediate,0),(Position,1),4))
-- (4,[],fromList [107,0,1,4,0])
--
-- >>> execInstruction (0, [], Seq.fromList [1007,0,1,4,0]) $ LessThan (Just ((Position,0),(Immediate,1),4))
-- (4,[],fromList [1007,0,1,4,0])
--
-- >>> execInstruction (0, [], Seq.fromList [1107,0,1,4,0]) $ LessThan (Just ((Immediate,0),(Immediate,1),4))
-- (4,[],fromList [1107,0,1,4,1])
--
-- >>> execInstruction (0, [], Seq.fromList [8,0,0,3]) $ Equals (Just ((Position,0),(Position,0),3))
-- (4,[],fromList [8,0,0,1])
--
-- >>> execInstruction (0, [], Seq.fromList [108,0,0,3]) $ Equals (Just ((Immediate,0),(Position,0),3))
-- (4,[],fromList [108,0,0,0])
--
-- >>> execInstruction (0, [], Seq.fromList [1008,0,0,3]) $ Equals (Just ((Position,0),(Immediate,0),3))
-- (4,[],fromList [1008,0,0,0])
--
-- >>> execInstruction (0, [], Seq.fromList [1108,0,0,3]) $ Equals (Just ((Immediate,0),(Immediate,0),3))
-- (4,[],fromList [1108,0,0,1])
--
-- >>> execInstruction (0, [], Seq.fromList [104,2,0]) $ Print (Just (Immediate,2))
-- (2,[2],fromList [104,2,0])
--
-- >>> execInstruction (0, [], Seq.fromList [1002,4,3,4,33]) $ Mul (Just ((Position,4),(Immediate,3),4))
-- (4,[],fromList [1002,4,3,4,99])
execInstruction :: (Int, [Int], Seq Int) -> Instruction -> (Int, [Int], Seq Int)
execInstruction (c, prints, xs) (Add (Just (para1, para2, target))) = (c + 1 + 3, prints, xs & ix target .~ uncurry (+) (getPosPair xs para1 para2))
execInstruction (c, prints, xs) (Mul (Just (para1, para2, target))) = (c + 1 + 3, prints, xs & ix target .~ uncurry (*) (getPosPair xs para1 para2))
execInstruction (c, prints, xs) (Save (Just (target, val))) = (c + 1 + 1, prints, xs & ix target .~ val)
execInstruction (c, prints, xs) (Print (Just para)) = (c + 1 + 1, prints ++ [getPos xs para], xs)
execInstruction (c, prints, xs) (JumpIfTrue (Just (para1, para2))) =
  case getPosPair xs para1 para2 of
    (0, v2) -> (c + 1 + 2, prints, xs)
    (_, v2) -> (v2, prints, xs)
execInstruction (c, prints, xs) (JumpIfFalse (Just (para1, para2))) =
  case getPosPair xs para1 para2 of
    (0, v2) -> (v2, prints, xs)
    (_, v2) -> (c + 1 + 2, prints, xs)
execInstruction (c, prints, xs) (LessThan (Just (para1, para2, target))) = (c + 1 + 3, prints, xs & ix target .~ bool 0 1 (uncurry (<) $ getPosPair xs para1 para2))
execInstruction (c, prints, xs) (Equals (Just (para1, para2, target))) = (c + 1 + 3, prints, xs & ix target .~ bool 0 1 (uncurry (==) $ getPosPair xs para1 para2))

-- |
--
-- >>> toInstruction Nothing $ Seq.fromList [1,2,4,6]
-- Add (Just ((Position,2),(Position,4),6))
--
-- >>> toInstruction Nothing $ Seq.fromList [1001,2,4,6]
-- Add (Just ((Position,2),(Immediate,4),6))
--
-- >>> toInstruction Nothing $ Seq.fromList [1101,2,4,6]
-- Add (Just ((Immediate,2),(Immediate,4),6))
--
-- >>> toInstruction Nothing $ Seq.fromList [2,4,3,4]
-- Mul (Just ((Position,4),(Position,3),4))
--
-- >>> toInstruction Nothing $ Seq.fromList [1002,4,3,4]
-- Mul (Just ((Position,4),(Immediate,3),4))
--
-- >>> toInstruction Nothing $ Seq.fromList [1102,4,3,4]
-- Mul (Just ((Immediate,4),(Immediate,3),4))
--
-- >>> toInstruction (Just 55) $ Seq.fromList [3,10]
-- Save (Just (10,55))
--
-- >>> toInstruction Nothing $ Seq.fromList [4,2]
-- Print (Just (Position,2))
--
-- >>> toInstruction Nothing $ Seq.fromList [104,2]
-- Print (Just (Immediate,2))
--
-- >>> toInstruction Nothing $ Seq.fromList [5,0,1]
-- JumpIfTrue (Just ((Position,0),(Position,1)))
--
-- >>> toInstruction Nothing $ Seq.fromList [105,0,1]
-- JumpIfTrue (Just ((Immediate,0),(Position,1)))
--
-- >>> toInstruction Nothing $ Seq.fromList [1005,0,1]
-- JumpIfTrue (Just ((Position,0),(Immediate,1)))
--
-- >>> toInstruction Nothing $ Seq.fromList [1105,0,1]
-- JumpIfTrue (Just ((Immediate,0),(Immediate,1)))
--
-- >>> toInstruction Nothing $ Seq.fromList [6,0,1]
-- JumpIfFalse (Just ((Position,0),(Position,1)))
--
-- >>> toInstruction Nothing $ Seq.fromList [106,0,1]
-- JumpIfFalse (Just ((Immediate,0),(Position,1)))
--
-- >>> toInstruction Nothing $ Seq.fromList [1006,0,1]
-- JumpIfFalse (Just ((Position,0),(Immediate,1)))
--
-- >>> toInstruction Nothing $ Seq.fromList [1106,0,1]
-- JumpIfFalse (Just ((Immediate,0),(Immediate,1)))
--
-- >>> toInstruction Nothing $ Seq.fromList [7,0,1,4]
-- LessThan (Just ((Position,0),(Position,1),4))
--
-- >>> toInstruction Nothing $ Seq.fromList [107,0,1,4]
-- LessThan (Just ((Immediate,0),(Position,1),4))
--
-- >>> toInstruction Nothing $ Seq.fromList [1007,0,1,4]
-- LessThan (Just ((Position,0),(Immediate,1),4))
--
-- >>> toInstruction Nothing $ Seq.fromList [1107,0,1,4]
-- LessThan (Just ((Immediate,0),(Immediate,1),4))
--
-- >>> toInstruction Nothing $ Seq.fromList [8,0,0,3]
-- Equals (Just ((Position,0),(Position,0),3))
--
-- >>> toInstruction Nothing $ Seq.fromList [108,0,0,3]
-- Equals (Just ((Immediate,0),(Position,0),3))
--
-- >>> toInstruction Nothing $ Seq.fromList [1008,0,0,3]
-- Equals (Just ((Position,0),(Immediate,0),3))
--
-- >>> toInstruction Nothing $ Seq.fromList [1108,0,0,3]
-- Equals (Just ((Immediate,0),(Immediate,0),3))
--
-- >>> toInstruction Nothing $ Seq.fromList [99]
-- Terminate
--
-- >>> toInstruction Nothing $ Seq.fromList [100]
-- Error (fromList [100])
toInstruction :: Maybe Int -> Seq Int -> Instruction
toInstruction _ (extOpCode :<| para1 :<| para2 :<| Empty) =
  case parseExtOpCode extOpCode of
    (JumpIfTrue Nothing, m1 : m2 : _) -> JumpIfTrue $ Just ((m1, para1), (m2, para2))
    (JumpIfTrue Nothing, m1 : _) -> JumpIfTrue $ Just ((m1, para1), (Position, para2))
    (JumpIfTrue Nothing, []) -> JumpIfTrue $ Just ((Position, para1), (Position, para2))
    (JumpIfFalse Nothing, m1 : m2 : _) -> JumpIfFalse $ Just ((m1, para1), (m2, para2))
    (JumpIfFalse Nothing, m1 : _) -> JumpIfFalse $ Just ((m1, para1), (Position, para2))
    (JumpIfFalse Nothing, []) -> JumpIfFalse $ Just ((Position, para1), (Position, para2))
toInstruction _ (extOpCode :<| para1 :<| para2 :<| target :<| Empty) =
  case parseExtOpCode extOpCode of
    (Add Nothing, m1 : m2 : _) -> Add $ Just ((m1, para1), (m2, para2), target)
    (Add Nothing, m1 : _) -> Add $ Just ((m1, para1), (Position, para2), target)
    (Add Nothing, []) -> Add $ Just ((Position, para1), (Position, para2), target)
    (Mul Nothing, m1 : m2 : _) -> Mul $ Just ((m1, para1), (m2, para2), target)
    (Mul Nothing, m1 : _) -> Mul $ Just ((m1, para1), (Position, para2), target)
    (Mul Nothing, []) -> Mul $ Just ((Position, para1), (Position, para2), target)
    (LessThan Nothing, m1 : m2 : _) -> LessThan $ Just ((m1, para1), (m2, para2), target)
    (LessThan Nothing, m1 : _) -> LessThan $ Just ((m1, para1), (Position, para2), target)
    (LessThan Nothing, []) -> LessThan $ Just ((Position, para1), (Position, para2), target)
    (Equals Nothing, m1 : m2 : _) -> Equals $ Just ((m1, para1), (m2, para2), target)
    (Equals Nothing, m1 : _) -> Equals $ Just ((m1, para1), (Position, para2), target)
    (Equals Nothing, []) -> Equals $ Just ((Position, para1), (Position, para2), target)
    (_, _) -> error $ "invalid opCode " ++ show extOpCode
toInstruction inputVal (extOpCode :<| para1 :<| Empty) =
  case (parseExtOpCode extOpCode, inputVal) of
    ((Save Nothing, _), Just v) -> Save $ Just (para1, v)
    ((Save Nothing, _), Nothing) -> error "Save: please provide input value; none provided!"
    ((Print Nothing, m1 : _), _) -> Print $ Just (m1, para1)
    ((Print Nothing, []), _) -> Print $ Just (Position, para1)
toInstruction _ (99 :<| Empty) = Terminate
toInstruction _ xs = Error xs

toBaseInstruction :: OpCode -> Instruction
toBaseInstruction 1 = Add Nothing
toBaseInstruction 2 = Mul Nothing
toBaseInstruction 3 = Save Nothing
toBaseInstruction 4 = Print Nothing
toBaseInstruction 5 = JumpIfTrue Nothing
toBaseInstruction 6 = JumpIfFalse Nothing
toBaseInstruction 7 = LessThan Nothing
toBaseInstruction 8 = Equals Nothing
toBaseInstruction 99 = Terminate
toBaseInstruction xs = error $ "invalid opCode " ++ show xs

parse :: (Seq (Seq Int), Seq Int) -> (Seq (Seq Int), Seq Int)
parse (done, Empty) = (done, Empty)
parse (done, todo@(x :<| _)) = parse (done |> moving, remaining)
  where
    parametricity x = case getOpCode x of
      1 -> 1 + 3
      2 -> 1 + 3
      3 -> 1 + 1
      4 -> 1 + 1
      5 -> 1 + 2
      6 -> 1 + 2
      7 -> 1 + 3
      8 -> 1 + 3
      99 -> 1
      _ -> 1
    moving = Seq.take (parametricity x) todo
    remaining = Seq.drop (parametricity x) todo

getPos :: Seq Int -> (Mode, Int) -> Int
getPos xs (Position, x) = fromMaybe 0 $ Seq.lookup x xs
getPos _ (Immediate, x) = x

getPosPair :: Seq Int -> (Mode, Int) -> (Mode, Int) -> (Int, Int)
getPosPair xs para1 para2 = (getPos xs para1, getPos xs para2)

-- | Core logic.
--
-- >>> umbrella Nothing (0, [], Seq.fromList [1,0,0,0,99])
-- (4,[],fromList [2,0,0,0,99])
--
-- >>> umbrella Nothing (0, [], Seq.fromList [2,3,0,3,99])
-- (4,[],fromList [2,3,0,6,99])
--
-- >>> umbrella Nothing (0, [], Seq.fromList [2,4,4,5,99,0])
-- (4,[],fromList [2,4,4,5,99,9801])
--
-- >>> umbrella Nothing (0, [], Seq.fromList [1,1,1,4,99,5,6,0,99])
-- (8,[],fromList [30,1,1,4,2,5,6,0,99])
--
-- >>> umbrella Nothing (0, [], Seq.fromList [1101,100,-1,4,0])
-- (4,[],fromList [1101,100,-1,4,99])
umbrella :: Maybe Val -> (Int, [Int], Seq Int) -> (Int, [Int], Seq Int)
umbrella userInput (c, prints, xs) =
  case nextInstruction userInput (Seq.drop c xs) of
    Just Terminate -> (c, prints, xs)
    Just (Error (x :<| Empty)) -> error $ "invalid opCode " ++ show x
    Just (Error xs) -> error $ "invalid opCode " ++ show xs
    Just istr -> umbrella userInput $ execInstruction (c, prints, xs) istr
    Nothing -> (c, prints, xs)

nextInstruction :: Maybe Val -> Seq Int -> Maybe Instruction
nextInstruction userInput = seqToMaybe . fmap (toInstruction userInput) . fst . parse . (,) Empty

seqToMaybe :: Seq a -> Maybe a
seqToMaybe Empty = Nothing
seqToMaybe (x :<| _) = Just x

-- Day 5.2

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
getOpCode x = x `mod` 10

-- |
--
-- >>> modes [1,1,0,0,2]
-- [1,1,0]
--
-- >>> modes [1,0,0,2]
-- [1,0]
--
-- >>> modes [1,0,4]
-- [1]
modes :: [Int] -> [Int]
modes [_] = []
modes [_, _] = []
modes [y, _, _] = [y]
modes [x, y, _, _] = [x, y]
modes [w, x, y, _, _] = [w, x, y]

-- |
--
-- >>> parseExtOpCode 1
-- (Add Nothing,[])
--
-- >>> parseExtOpCode 2
-- (Mul Nothing,[])
--
-- >>> parseExtOpCode 102
-- (Mul Nothing,[Immediate])
--
-- >>> parseExtOpCode 1002
-- (Mul Nothing,[Position,Immediate])
--
-- >>> parseExtOpCode 11002
-- (Mul Nothing,[Position,Immediate,Immediate])
--
-- >>> parseExtOpCode 3
-- (Save Nothing,[])
--
-- >>> parseExtOpCode 4
-- (Print Nothing,[])
--
-- >>> parseExtOpCode 104
-- (Print Nothing,[Immediate])
--
-- >>> parseExtOpCode 99
-- (Terminate,[])
parseExtOpCode :: ExtOpCode -> (Instruction, [Mode])
parseExtOpCode xs
  | xs < 100 = (toBaseInstruction $ getOpCode xs, [])
  | otherwise = (toBaseInstruction $ getOpCode xs, reverse . map toMode . modes . digits $ xs)

toMode :: ExtOpCode -> Mode
toMode 0 = Position
toMode 1 = Immediate
toMode x = error $ "invalid mode specified by " ++ show x
