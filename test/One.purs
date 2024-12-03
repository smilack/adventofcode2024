module Test.AdventOfCode.Twenty24.One
  ( main
  ) where

import Prelude

import AdventOfCode.Twenty24.One (distances, pairUp, parse, solve1, sort)
import AdventOfCode.Twenty24.One.Pair (Pair(..))
import Data.List.Types (List(..), (:))
import Effect (Effect)
import Test.Spec (describe, it, pending)
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
      pending "more stuff"

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
3   9
3   3"""
  , output: Pair
      { a: 3 : 4 : 2 : 1 : 3 : 3 : Nil
      , b: 4 : 3 : 5 : 3 : 9 : 3 : Nil
      }
  }

sortTest :: { input :: Pair (List Int), output :: Pair (List Int) }
sortTest =
  { input: inputTest.output
  , output: Pair
      { a: 1 : 2 : 3 : 3 : 3 : 4 : Nil
      , b: 3 : 3 : 3 : 4 : 5 : 9 : Nil
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
        : Pair { a: 4, b: 9 }
        : Nil
  }

distanceTest :: { input :: List (Pair Int), output :: List Int }
distanceTest =
  { input: sequenceTest.output
  , output: 2 : 1 : 0 : 1 : 2 : 5 : Nil
  }

puzzle1Test :: { input :: String, output :: Int }
puzzle1Test = inputTest { output = 11 }