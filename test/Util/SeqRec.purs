module Test.AdventOfCode.Twenty24.Util.SeqRec
  ( main
  ) where

import AdventOfCode.Prelude hiding (sequence)
import AdventOfCode.Twenty24.Util.SeqRec (class RecordOfAp)

import Effect (Effect)
import Test.QuickCheck ((===), Result)
import Test.Spec (pending, describe, it)
import Test.Spec.Assertions (shouldEqual, AnyShow(..))
import Test.Spec.QuickCheck (quickCheck)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner.Node (runSpecAndExitProcess)
import Type.RowList (Cons, Nil)

main :: Effect Unit
main = runSpecAndExitProcess [ consoleReporter ] do
  describe "TraversableRecord" do
    describe "RecordOfAp" do
      it "EmptyRecord" do
        recOfAp @Maybe @Nil {} `shouldEqual` (AnyShow {})
      pending "other stuff"

recOfAp
  :: forall @f r @l
   . RecordOfAp f r l
  => Record r
  -> AnyShow (Record r)
recOfAp = AnyShow