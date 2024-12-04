module Test.AdventOfCode.Twenty24.Three
  ( main
  ) where

import AdventOfCode.Twenty24.Three (compute, filterEnabled, parse, solve1, solve2)
import Prelude (Unit, discard)

import Data.List (List(..), (:))
import Data.Tuple (Tuple)
import Data.Tuple.Nested ((/\))
import Effect (Effect)
import Test.Spec (describe, it)
import Test.Spec.Assertions (shouldEqual)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner.Node (runSpecAndExitProcess)

main :: Effect Unit
main = runSpecAndExitProcess [ consoleReporter ] do
  describe "Day Three" do
    describe "Part 1" do
      it "Parses input" do
        parse input `shouldEqual` mulPairs
      it "Runs mul instructions" do
        compute mulPairs `shouldEqual` solution1
      it "Solves part 1" do
        solve1 input `shouldEqual` solution1
    describe "Part 2" do
      it "Eliminates disabled segments" do
        filterEnabled input2 `shouldEqual` enabled
      it "Solves part 2" do
        solve2 input2 `shouldEqual` solution2

input :: String
input =
  "xmul(2,4)%&mul[3,7]!@^do_not_mul(5,5)+mul(32,64]then(mul(11,8)mul(8,5))"

mulPairs :: List (Tuple Int Int)
mulPairs = (2 /\ 4) : (5 /\ 5) : (11 /\ 8) : (8 /\ 5) : Nil

solution1 :: Int
solution1 = 161

input2 :: String
input2 =
  "xmul(2,4)&mul[3,7]!^don't()_mul(5,5)+mul(32,64](mul(11,8)undo()?mul(8,5))"

enabled :: String
enabled = "xmul(2,4)&mul[3,7]!^?mul(8,5))"

solution2 :: Int
solution2 = 48