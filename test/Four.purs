module Test.AdventOfCode.Twenty24.Four
  ( main
  ) where

import Prelude
import AdventOfCode.Twenty24.Four
import AdventOfCode.Twenty24.Util
import Data.String (split)
import Data.String.Pattern (Pattern(..))
import Effect (Effect)
import Test.QuickCheck ((===), Result)
import Test.Spec (pending, describe, it)
import Test.Spec.Assertions (shouldEqual)
import Test.Spec.QuickCheck (quickCheck)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner.Node (runSpecAndExitProcess)

main :: Effect Unit
main = runSpecAndExitProcess [ consoleReporter ] do
  describe "Day Four" do
    describe "Part 1" do
      pending "parse input"
      pending "other stuff"
    describe "Part 2" do
      pending "more stuff"
