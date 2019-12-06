{-# LANGUAGE OverloadedLists #-}

module Day2 where

import Data.Maybe (fromMaybe)
import qualified Data.Sequence as Seq
import Data.Sequence ((<|), (><), Seq ((:<|), (:|>), Empty), (|>))
import IntCode
import Optics.At.Core (ix)
import Optics.Operators ((.~))
import Optics.Optic ((&))

prepare :: Int -> Int -> Seq Int -> Seq Int
prepare first second xs = xs & ix 1 .~ first & ix 2 .~ second

intCode :: (Seq Int -> Seq Int) -> Seq Int -> Int
intCode preparer = fromMaybe 0 . Seq.lookup 0 . trd' . umbrella Nothing . (,,) 0 [] . preparer
  where
    trd' (_, _, x) = x

getFirstOfIntCode :: Seq Int -> Int
getFirstOfIntCode = Day2.intCode (Day2.prepare 12 2)

findPair :: Seq Int -> Int -> Maybe (Int, Int)
findPair is z = seqToMaybe $ Seq.filter (\(x, y) -> intCode (prepare x y) is == z) $ Seq.fromList [(x, y) | x <- [0 .. 99], y <- [0 .. 99]]

formatPair :: Int -> Int -> Int
formatPair noun verb = 100 * noun + verb
