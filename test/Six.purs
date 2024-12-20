module Test.AdventOfCode.Twenty24.Six
  ( main
  ) where

import AdventOfCode.Prelude

import AdventOfCode.Twenty24.Six (initialState, run, solve1, toMap)
import Effect (Effect)
import Effect.Class.Console (logShow)
import Test.QuickCheck ((===), Result)
import Test.Spec (pending, describe, it)
import Test.Spec.Assertions (shouldEqual)
import Test.Spec.QuickCheck (quickCheck)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner.Node (runSpecAndExitProcess)

main :: Effect Unit
main = do
  logShow $ toMap testInput
  runSpecAndExitProcess [ consoleReporter ] do
    describe "Day Six" do
      describe "Part 1" do
        it "solve 1" do
          solve1 testInput `shouldEqual` 41
      describe "Part 2" do
        pending "more stuff"

testInput :: String
testInput =
  "....#.....\n.........#\n..........\n..#.......\n.......#..\n..........\n.#..^.....\n........#.\n#.........\n......#..."

testOut :: String
testOut =
  """░░░░█░░░░░
░░░░     █
░░░░ ░░░ ░
░░█░ ░░░ ░
░░     █ ░
░░ ░ ░ ░ ░
░█       ░
░       █░
█       ░░
░░░░░░█ ░░"""
