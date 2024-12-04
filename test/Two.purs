module Test.AdventOfCode.Twenty24.Two
  ( main
  ) where

import Prelude

import AdventOfCode.Twenty24.Two (Safety(..), check, countSafe, doubleCheck, parse, solve1)
import Data.List (List(..), fromFoldable, (:))
import Data.List.NonEmpty (cons')
import Data.List.Types (NonEmptyList)
import Effect (Effect)
import Test.Spec (describe, it)
import Test.Spec.Assertions (shouldEqual)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner.Node (runSpecAndExitProcess)

main :: Effect Unit
main = runSpecAndExitProcess [ consoleReporter ] do
  describe "Day Two" do
    describe "Part 1" do
      it "Parses input 1" do
        parse input `shouldEqual` reports
      it "Parses input 2" do
        parse input2 `shouldEqual` reports
      it "Parses input 3" do
        parse input3 `shouldEqual` reports
      it "Checks reports" do
        map check reports `shouldEqual` safety
      it "Counts safe reports" do
        countSafe safety `shouldEqual` 2
      it "Solves part 1" do
        solve1 input `shouldEqual` 2
    describe "Part 2" do
      it "Double checks reports" do
        map doubleCheck double `shouldEqual` doublesafe

input :: String
input = "7 6 4 2 1\n1 2 7 8 9\n9 7 6 2 1\n1 3 2 4 5\n8 6 4 4 1\n1 3 6 7 9"

input2 :: String
input2 = "7 6 4 2 1\n1 2 7 8 9\n9 7 6 2 1\n1 3 2 4 5\n8 6 4 4 1\n1 3 6 7 9\n"

input3 :: String
input3 = "7 6 4 2 1\n1 2 7 8 9\n9 7 6 2 1\n1 3 2 4 5\n8 6 4 4 1\n1 3 6 7 9\n\n"

reports :: List (NonEmptyList Int)
reports = fromFoldable
  [ cons' 7 $ 6 : 4 : 2 : 1 : Nil
  , cons' 1 $ 2 : 7 : 8 : 9 : Nil
  , cons' 9 $ 7 : 6 : 2 : 1 : Nil
  , cons' 1 $ 3 : 2 : 4 : 5 : Nil
  , cons' 8 $ 6 : 4 : 4 : 1 : Nil
  , cons' 1 $ 3 : 6 : 7 : 9 : Nil
  ]

safety :: List Safety
safety = fromFoldable
  [ Safe
  , Unsafe
  , Unsafe
  , Unsafe
  , Unsafe
  , Safe
  ]

double :: List (NonEmptyList Int)
double = fromFoldable
  [ cons' 1 $ 2 : 7 : 8 : 9 : Nil
  , cons' 9 $ 7 : 6 : 2 : 1 : Nil
  , cons' 1 $ 3 : 2 : 4 : 5 : Nil
  , cons' 8 $ 6 : 4 : 4 : 1 : Nil
  ]

doublesafe :: List Safety
doublesafe = fromFoldable
  [ Unsafe
  , Unsafe
  , Safe
  , Safe
  ]