import qualified Day1
import qualified Day2
import qualified Day3
import qualified Day4
import qualified Day5
import qualified Day6
import qualified Day7
import qualified IntCode
import Test.Hspec

main :: IO ()
main = hspec $
  do
    describe "Day 1.2"
      $ it "calculates totalFuelRequirement correctly"
      $ do
        result <- Day1.totalFuelRequirement <$> Day1.getInput "inputs/1.txt"
        result `shouldBe` 4920708
    describe "Day 2.1"
      $ it "Intcode program is working"
      $ do
        result <- Day2.getFirstOfIntCode <$> IntCode.getInput "inputs/2.txt"
        result `shouldBe` 5866714
    describe "Day 2.2" $ do
      it "find pair (52,8) to produce output of 19690720" $ do
        is <- IntCode.getInput "inputs/2.txt"
        let pair = Day2.findPair is 19690720
        pair `shouldBe` Just (52, 8)
      it "format pair to produce output of 19690720 to 5208" $ do
        is <- IntCode.getInput "inputs/2.txt"
        let pair = Day2.findPair is 19690720
        maybe 0 (uncurry Day2.formatPair) pair `shouldBe` 5208
    describe "Day 3.1"
      $ it "calculates Manhattan distance to closest intersection (to central port) to 1211"
      $ do
        [a, b] <- Day3.getInput "inputs/3.txt"
        Day3.nearestIntersection a b `shouldBe` Just 1211
    describe "Day 3.2"
      $ it "determines fewest combined steps to intersection to result 101386"
      $ do
        [a, b] <- Day3.getInput "inputs/3.txt"
        Day3.fewestCombined a b `shouldBe` Just 101386
    describe "Day 4.1"
      $ it "determines number of different valid passwords in range 248345 through 746315 to 1019"
      $ Day4.countValidPasswords Day4.twoAdjacent 248345 746315 `shouldBe` 1019
    describe "Day 4.2"
      $ it "determines number of different valid passwords in range 248345 through 746315 to 660"
      $ Day4.countValidPasswords Day4.twoAdjacent' 248345 746315 `shouldBe` 660
    describe "Day 5.1"
      $ it "makes sure that all outputs are zero and resulting diagcode is 9025675"
      $ do
        (_, _, result, _) <- Day5.outputs 1 <$> IntCode.getInput "inputs/5.txt"
        all (== 0) (init result) `shouldBe` True
        last result `shouldBe` 9025675
    describe "Day 5.2"
      $ it "determines the output only to be the diagnostic code of 11981754"
      $ do
        (_, _, result, _) <- Day5.outputs 5 <$> IntCode.getInput "inputs/5.txt"
        result `shouldBe` [11981754]
    describe "Day 6.1"
      $ it "determines the total no. of orbits to 234446"
      $ do
        total <- Day6.countTotalOrbits <$> Day6.getInput "inputs/6.txt"
        total `shouldBe` 234446
    describe "Day 6.2"
      $ it "determines minimum number of orbital transfers to 385"
      $ do
        transfers <- Day6.detMinimumTransfers "SAN" "YOU" <$> Day6.getInput "inputs/6.txt"
        transfers `shouldBe` 385
    describe "Day 7.1"
      $ it "determines the maximum thruster signal to 87138"
      $ do
        maxThruster <- (Day7.detMaxThrusterSignal 0 [0..4]) <$> IntCode.getInput "inputs/7.txt"
        maxThruster `shouldBe` 87138
