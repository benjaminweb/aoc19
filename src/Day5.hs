{-# LANGUAGE OverloadedLists #-}

module Day5 where

import Data.Sequence (Seq ())
import IntCode

outputs :: Int -> Seq Int -> (Int, [Int], Seq Int)
outputs userInput xs = umbrella (Just userInput) (0, [], xs)
