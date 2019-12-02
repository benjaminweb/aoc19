import qualified Day1
import qualified Day2
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
        result <- Day2.intCode <$> Day2.getInput "inputs/2.txt"
        result `shouldBe` 5866714
