import qualified Day1
import qualified Day2
import qualified Day3
import qualified Day4
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
        result <- Day2.intCode (Day2.prepare 12 2) <$> Day2.getInput "inputs/2.txt"
        result `shouldBe` 5866714
    describe "Day 2.2" $ do
      it "find pair (52,8) to produce output of 19690720" $ do
        is <- Day2.getInput "inputs/2.txt"
        let pair = Day2.findPair is 19690720
        pair `shouldBe` Just (52, 8)
      it "format pair to produce output of 19690720 to 5208" $ do
        is <- Day2.getInput "inputs/2.txt"
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
