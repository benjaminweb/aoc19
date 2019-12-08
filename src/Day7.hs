module Day7 where

import Data.Foldable (foldl')
import Data.List (permutations)
import Data.Sequence (Seq ())
import IntCode

type Phase = Int

type Input = Int

type Output = Int

type MaxSignal = Int

amplifier :: Val -> Seq Int -> Phase -> Output
amplifier inputSignal program phase = let (_, _, [x], _) = umbrella ([phase, inputSignal], 0, [], program) in x

-- >>> maxThrusterSignal (Seq.fromList [3,15,3,16,1002,16,10,16,1,16,15,15,4,15,99,0,0]) 0 [4,3,2,1,0]
-- 43210
--
-- >>> maxThrusterSignal (Seq.fromList [3,23,3,24,1002,24,10,24,1002,23,-1,23,101,5,23,23,1,24,23,23,4,23,99,0,0]) 0 [0,1,2,3,4]
-- 54321
--
-- >>> maxThrusterSignal (Seq.fromList [3,31,3,32,1002,32,10,32,1001,31,-2,31,1007,31,0,33,1002,33,7,33,1,33,31,31,1,32,31,31,4,31,99,0,0,0]) 0 [1,0,4,3,2]
-- 65210
maxThrusterSignal :: Seq Int -> Input -> [Phase] -> MaxSignal
maxThrusterSignal program = foldl' (\acc x -> amplifier acc program x)

detMaxThrusterSignal :: Input -> Seq Int -> MaxSignal
detMaxThrusterSignal inputSignal program = maximum $ map (maxThrusterSignal program inputSignal) $ permutations [0 .. 4]
