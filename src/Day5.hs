{-# LANGUAGE OverloadedLists #-}

module Day5 where

import Data.Sequence (Seq ())
import IntCode

outputs :: Int -> Seq Int -> ([Val], Int, [Int], Seq Int)
outputs userInput xs = umbrella ([userInput], 0, [], xs)
