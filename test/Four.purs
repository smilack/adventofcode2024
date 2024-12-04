module Test.AdventOfCode.Twenty24.Four
  ( main
  ) where

import Prelude

import AdventOfCode.Twenty24.Four (Point(..), parse, solve1, solve2)
import Data.Map (Map)
import Data.Map as Map
import Data.Tuple.Nested ((/\))
import Effect (Effect)
import Test.Spec (describe, it)
import Test.Spec.Assertions (shouldEqual)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner.Node (runSpecAndExitProcess)

main :: Effect Unit
main = runSpecAndExitProcess [ consoleReporter ] do
  describe "Day Four" do
    describe "Part 1" do
      it "parses input" do
        parse parseTest.input `shouldEqual` parseTest.output
        parse parseTest2.input `shouldEqual` parseTest2.output
      it "Solves part 1" do
        solve1 input `shouldEqual` solution1
    describe "Part 2" do
      it "Solves mini part 2" do
        solve2 miniSolution2.input `shouldEqual` miniSolution2.output
      it "Solves part 2" do
        solve2 solution2.input `shouldEqual` solution2.output

parseTest :: { input :: String, output :: Map Point Char }
parseTest =
  { input: "MMM\nMSA\nAMX\n"
  , output: Map.fromFoldable
      [ (Point { x: 0, y: 0 }) /\ 'M'
      , (Point { x: 1, y: 0 }) /\ 'M'
      , (Point { x: 2, y: 0 }) /\ 'M'
      , (Point { x: 0, y: 1 }) /\ 'M'
      , (Point { x: 1, y: 1 }) /\ 'S'
      , (Point { x: 2, y: 1 }) /\ 'A'
      , (Point { x: 0, y: 2 }) /\ 'A'
      , (Point { x: 1, y: 2 }) /\ 'M'
      , (Point { x: 2, y: 2 }) /\ 'X'
      ]
  }

input :: String
input =
  "MMMSXXMASM\nMSAMXMSMSA\nAMXSXMAAMM\nMSAMASMSMX\nXMASAMXAMM\nXXAMMXXAMA\nSMSMSASXSS\nSAXAMASAAA\nMAMMMXMMMM\nMXMXAXMASX\n"

-- MMMSXXMASM
-- MSAMXMSMSA
-- AMXSXMAAMM
-- MSAMASMSMX
-- XMASAMXAMM
-- XXAMMXXAMA
-- SMSMSASXSS
-- SAXAMASAAA
-- MAMMMXMMMM
-- MXMXAXMASX

solution1 :: Int
solution1 = 18

parseTest2 :: { input :: String, output :: Map Point Char }
parseTest2 =
  { input: "MMMS\nMSAM\nAMXS\nMSAM"
  , output: Map.fromFoldable
      [ (Point { x: 0, y: 0 }) /\ 'M'
      , (Point { x: 1, y: 0 }) /\ 'M'
      , (Point { x: 2, y: 0 }) /\ 'M'
      , (Point { x: 3, y: 0 }) /\ 'S'
      , (Point { x: 0, y: 1 }) /\ 'M'
      , (Point { x: 1, y: 1 }) /\ 'S'
      , (Point { x: 2, y: 1 }) /\ 'A'
      , (Point { x: 3, y: 1 }) /\ 'M'
      , (Point { x: 0, y: 2 }) /\ 'A'
      , (Point { x: 1, y: 2 }) /\ 'M'
      , (Point { x: 2, y: 2 }) /\ 'X'
      , (Point { x: 3, y: 2 }) /\ 'S'
      , (Point { x: 0, y: 3 }) /\ 'M'
      , (Point { x: 1, y: 3 }) /\ 'S'
      , (Point { x: 2, y: 3 }) /\ 'A'
      , (Point { x: 3, y: 3 }) /\ 'M'
      ]
  }

miniSolution2 :: { input :: String, output :: Int }
miniSolution2 =
  { input: parseTest2.input
  , output: 1
  }

solution2 :: { input :: String, output :: Int }
solution2 =
  { input
  , output: 9
  }
