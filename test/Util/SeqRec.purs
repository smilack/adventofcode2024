module Test.AdventOfCode.Twenty24.Util.SeqRec
  ( main
  ) where

import AdventOfCode.Prelude hiding (sequence)
import AdventOfCode.Twenty24.Util.SeqRec (class RecordOfAp, class RecordOfAp)

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
      it "One field" do
        recOfAp { foo: Just 1 }
          `shouldEqual` (AnyShow { foo: Just 1 })
      -- it "Two fields" do
      -- recOfAp { foo: Just 1, bar: Just 5 }
      --   `shouldEqual` (AnyShow { foo: Just 1, bar: Just 5 })
      pending "other stuff"

type FooMaybe = (foo :: Maybe Int)
type Foo = (foo :: Int)

-- This function basically exists to check the constraints
--   of RecordOfAp. There are tests for it, but if it compiles
--   it's kind of already succeeded.
recOfAp
  :: forall @f r
   . Applicative f
  => RecordOfAp f r
  => r
  -> AnyShow r
recOfAp = AnyShow