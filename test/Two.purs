module Test.AdventOfCode.Twenty24.Two
  ( main
  ) where

import Prelude
import AdventOfCode.Twenty24.Two
import AdventOfCode.Twenty24.Util
import Data.String (split)
import Data.String.Pattern (Pattern(..))
import Effect (Effect)
import Effect.Aff (launchAff_)
import Test.QuickCheck ((===), Result)
import Test.Spec (Spec, pending, describe, it)
import Test.Spec.Assertions (shouldEqual)
import Test.Spec.QuickCheck (quickCheck)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner (runSpec)

main :: Effect Unit
main = launchAff_ $ runSpec [ consoleReporter ] do
  describe "Day Two" do
    describe "Part 1" do
      pending "parse input"
      pending "other stuff"
    describe "Part 2" do
      pending "more stuff"