import qualified Day1
import Test.Hspec

main :: IO ()
main = hspec
  $ describe "Day 1.1"
  $ it "calculates totalFuelRequirement correctly"
  $ do
    result <- Day1.totalFuelRequirement <$> Day1.getInput "inputs/1.1.txt"
    result `shouldBe` 3282386
