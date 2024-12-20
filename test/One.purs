module Test.AdventOfCode.Twenty24.One
  ( main
  ) where

import Prelude

import AdventOfCode.Twenty24.One (distances, pairUp, parse, solve1, sort, solve2)
import AdventOfCode.Twenty24.One.Pair (Pair(..))
import AdventOfCode.Twenty24.Util (tally)
import Data.List.Types (List(..), (:))
import Data.Map (Map, fromFoldable)
import Data.Tuple.Nested ((/\))
import Effect (Effect)
import Test.Spec (describe, it)
import Test.Spec.Assertions (shouldEqual)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner.Node (runSpecAndExitProcess)

main :: Effect Unit
main = runSpecAndExitProcess [ consoleReporter ] do
  describe "Day One" do
    describe "Part 1" do
      it "Parses input" do
        parse parseTest.input `shouldEqual` parseTest.output
        parse inputTest.input `shouldEqual` inputTest.output
      it "Sorts lists" do
        (sort sortTest.input) `shouldEqual` sortTest.output
      it "Inverts list/pair" do
        (pairUp sequenceTest.input) `shouldEqual` sequenceTest.output
      it "Calculates distances" do
        (distances distanceTest.input) `shouldEqual` distanceTest.output
      it "Solves part 1" do
        solve1 puzzle1Test.input `shouldEqual` puzzle1Test.output
    describe "Part 2" do
      it "Counts occurrences in the second list" do
        tally countTest.input `shouldEqual` countTest.output
      it "Solves part 2" do
        solve2 puzzle2Test.input `shouldEqual` puzzle2Test.output

-- Shared

parseTest :: { input :: String, output :: Pair (List Int) }
parseTest =
  { input: "3   4\n4   3"
  , output: Pair { a: 3 : 4 : Nil, b: 4 : 3 : Nil }
  }

inputTest :: { input :: String, output :: Pair (List Int) }
inputTest =
  { input:
      """3   4
4   3
2   5
1   3
3   10
3   3
"""
  , output: Pair
      { a: 3 : 4 : 2 : 1 : 3 : 3 : Nil
      , b: 4 : 3 : 5 : 3 : 10 : 3 : Nil
      }
  }

-- Part 1

sortTest :: { input :: Pair (List Int), output :: Pair (List Int) }
sortTest =
  { input: inputTest.output
  , output: Pair
      { a: 1 : 2 : 3 : 3 : 3 : 4 : Nil
      , b: 3 : 3 : 3 : 4 : 5 : 10 : Nil
      }
  }

sequenceTest :: { input :: Pair (List Int), output :: List (Pair Int) }
sequenceTest =
  { input: sortTest.output
  , output:
      Pair { a: 1, b: 3 }
        : Pair { a: 2, b: 3 }
        : Pair { a: 3, b: 3 }
        : Pair { a: 3, b: 4 }
        : Pair { a: 3, b: 5 }
        : Pair { a: 4, b: 10 }
        : Nil
  }

distanceTest :: { input :: List (Pair Int), output :: List Int }
distanceTest =
  { input: sequenceTest.output
  , output: 2 : 1 : 0 : 1 : 2 : 6 : Nil
  }

puzzle1Test :: { input :: String, output :: Int }
puzzle1Test = inputTest { output = 12 }

-- Part 2

countTest :: { input :: List Int, output :: Map Int Int }
countTest =
  { input: 3 : 3 : 3 : 4 : 5 : 10 : Nil
  , output: fromFoldable [ 3 /\ 3, 4 /\ 1, 5 /\ 1, 10 /\ 1 ]
  }

puzzle2Test :: { input :: String, output :: Int }
puzzle2Test = inputTest { output = 31 }
