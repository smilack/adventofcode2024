module Test.AdventOfCode.Twenty24.Six
  ( main
  ) where

import Test.AdventOfCode.Prelude

-- import AdventOfCode.Twenty24.Six ()
-- import AdventOfCode.Twenty24.Util ()
import Effect (Effect)
import Test.QuickCheck ((===), Result)
import Test.Spec (pending, describe, it)
import Test.Spec.Assertions (shouldEqual)
import Test.Spec.QuickCheck (quickCheck)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner.Node (runSpecAndExitProcess)

main :: Effect Unit
main = runSpecAndExitProcess [ consoleReporter ] do
  describe "Day Six" do
    describe "Part 1" do
      pending "parse input"
      pending "other stuff"
    describe "Part 2" do
      pending "more stuff"