module Test.AdventOfCode.Twenty24.Six
  ( main
  ) where

import AdventOfCode.Prelude

import AdventOfCode.Twenty24.Six (possibleCoords, LabCell(..), initialState, isRect, run, solve1, solve2, toMap)
import AdventOfCode.Util.Area (set) as A
import AdventOfCode.Util.Coord (mkCoordRC)
import Data.FoldableWithIndex (foldMapWithIndex)
import Data.List (singleton)
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
  let
    testMap = toMap testInput
    part2ans = possibleCoords testInput
    part2poss = foldr (\coord -> A.set coord Visited) testMap part2ans
  logShow $ testMap
  logShow
    $ foldMapWithIndex
        ( \i -> case _ of
            Obstructed -> singleton i
            _ -> Nil
        )
    $ testMap
  log testOut
  logShow $ possibleCoords testInput
  logShow $ part2poss
  --
  runSpecAndExitProcess [ consoleReporter ] do
    describe "Day Six" do
      describe "Part 1" do
        it "solve 1" $ solve1 testInput `shouldEqual` 41
      describe "Part 2" do
        describe "isRect" do
          it "n" $ isRect r0c4 r1c9 r3c2 r4c7 `shouldEqual` false
          it "n" $ isRect r6c1 r7c8 r8c0 r9c6 `shouldEqual` false
          it "y" $ isRect r6c1 r7c8 r8c0 (mkCoordRC 9 7) `shouldEqual` true
          it "n" $ isRect r0c4 r3c2 r6c1 r8c0 `shouldEqual` false
        it "solve 2" $ solve2 testInput `shouldEqual` 6

testInput :: String
testInput =
  "....#.....\n.........#\n..........\n..#.......\n.......#..\n..........\n.#..^.....\n........#.\n#.........\n......#..."

testParsed :: String
testParsed =
  """░░░░█░░░░░
░░░░░░░░░█
░░░░░░░░░░
░░█░░░░░░░
░░░░░░░█░░
░░░░░░░░░░
░█░░↑░░░░░
░░░░░░░░█░
█░░░░░░░░░
░░░░░░█░░░"""

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

r0c4 = mkCoordRC 0 4
r1c9 = mkCoordRC 1 9
r3c2 = mkCoordRC 3 2
r4c7 = mkCoordRC 4 7
r6c1 = mkCoordRC 6 1
r7c8 = mkCoordRC 7 8
r8c0 = mkCoordRC 8 0
r9c6 = mkCoordRC 9 6
