
nextInstruction [1,1,1,4,99,5,6,0,99] -- start
Just (Instruction {iOp = Add, iSrcPos1 = 1, iSrcPos2 = 1, iStoragePos = 4})
execInstruction (0, [1,1,1,4,99,5,6,0,99]) Instruction {iOp = Add, iSrcPos1 = 1, iSrcPos2 = 1, iStoragePos = 4}
(4,[1,1,1,4,2,5,6,0,99])
nextInstruction $ drop 4 [1,1,1,4,2,5,6,0,99]
Just (Instruction {iOp = Mul, iSrcPos1 = 5, iSrcPos2 = 6, iStoragePos = 0})
execInstruction (4, [1,1,1,4,2,5,6,0,99]) (Instruction {iOp = Mul, iSrcPos1 = 5, iSrcPos2 = 6, iStoragePos = 0})
(4,[30,1,1,4,2,5,6,0,99])
nextInstruction $ drop 8 [30,1,1,4,2,5,6,0,99]
Just Terminate
-- return
[30,1,1,4,2,5,6,0,99]


