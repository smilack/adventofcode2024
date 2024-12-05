module Test.AdventOfCode.Twenty24.Five
  ( main
  ) where

import Test.AdventOfCode.Prelude

import AdventOfCode.Twenty24.Five (Manual(..), Page, Pages, Rule, combine, manual, middle, puzzleInput, rule)
import AdventOfCode.Twenty24.Util (multiline, testParser)
import Control.Monad.Error.Class (class MonadThrow)
import Data.List (List(..), (:))
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Tuple.Nested (type (/\), (/\))
import Effect (Effect)
import Effect.Exception (Error)
import Parsing.Combinators (many)
import Test.QuickCheck ((===), Result)
import Test.Spec (pending, describe, it)
import Test.Spec.Assertions (shouldEqual)
import Test.Spec.QuickCheck (quickCheck)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner.Node (runSpecAndExitProcess)

main :: Effect Unit
main = runSpecAndExitProcess [ consoleReporter ] do
  describe "Day Five" do
    describe "Part 1" do
      describe "Finds middle element of a list" do
        it "odd list" do
          test middleTest.a middle
        it "even list" do
          test middleTest.b middle
        it "empty list" do
          test middleTest.c middle
        it "singleton list" do
          test middleTest.d middle
      describe "parses input" do
        it "rules" do
          testParser parserTest.a.input parserTest.a.output (many rule)
        it "Consolidates rules" do
          combine ruleList `shouldEqual` ruleMap
        it "manuals" do
          testParser parserTest.b.input parserTest.b.output (multiline manual)
        it "full input" do
          testParser parserTest.c.input parserTest.c.output puzzleInput
      pending "other stuff"
    describe "Part 2" do
      pending "more stuff"

type Test a b = { input :: a, output :: b }

test
  :: forall (a :: Type) (m :: Type -> Type) (t :: Type)
   . MonadThrow Error m
  => Show t
  => Eq t
  => Test a t
  -> (a -> t)
  -> m Unit
test { input, output } f = f input `shouldEqual` output

type MiddleTest a = { input :: List a, output :: a }

middleTest
  :: { a :: Test (List Int) (Maybe Int)
     , b :: Test (List Int) (Maybe Int)
     , c :: Test (List Int) (Maybe Int)
     , d :: Test (List Int) (Maybe Int)
     }
middleTest =
  { a: { input: 16 : 91 : 15 : 29 : 46 : 17 : 67 : Nil, output: Just 29 }
  , b: { input: 16 : 91 : 15 : 29 : 46 : 17 : Nil, output: Nothing }
  , c: { input: Nil, output: Nothing }
  , d: { input: 1 : Nil, output: Just 1 }
  }

mapTest :: { input :: String, output :: Map Page Pages }
mapTest = { input: inputStrings.a, output: ruleMap }

parserTest
  :: { a :: Test String (List Rule)
     , b :: Test String (List Manual)
     , c :: Test String (Map Page Pages /\ List Manual)
     }
parserTest =
  { a: { input: inputStrings.a, output: ruleList }
  , b: { input: inputStrings.b, output: manuals }
  , c: { input: inputStrings.ab, output: ruleMap /\ manuals }
  }

inputStrings :: { a :: String, b :: String, ab :: String }
inputStrings =
  { a:
      "47|53\n97|13\n97|61\n97|47\n75|29\n61|13\n75|53\n29|13\n97|29\n53|29\n61|53\n97|53\n61|29\n47|13\n75|47\n97|75\n47|61\n75|61\n47|29\n75|13\n53|13\n"
  , b:
      "75,47,61,53,29\n97,61,53,29,13\n75,29,13\n75,97,47,61,53\n61,13,29\n97,13,75,29,47"
  , ab:
      "47|53\n97|13\n97|61\n97|47\n75|29\n61|13\n75|53\n29|13\n97|29\n53|29\n61|53\n97|53\n61|29\n47|13\n75|47\n97|75\n47|61\n75|61\n47|29\n75|13\n53|13\n\n75,47,61,53,29\n97,61,53,29,13\n75,29,13\n75,97,47,61,53\n61,13,29\n97,13,75,29,47\n"
  }

manuals :: List Manual
manuals = Manual (75 : 47 : 61 : 53 : 29 : Nil)
  : Manual (97 : 61 : 53 : 29 : 13 : Nil)
  : Manual (75 : 29 : 13 : Nil)
  : Manual (75 : 97 : 47 : 61 : 53 : Nil)
  : Manual (61 : 13 : 29 : Nil)
  : Manual (97 : 13 : 75 : 29 : 47 : Nil)
  : Nil

ruleList :: List Rule
ruleList = (47 /\ 53) : (97 /\ 13) : (97 /\ 61) : (97 /\ 47) : (75 /\ 29)
  : (61 /\ 13)
  : (75 /\ 53)
  : (29 /\ 13)
  : (97 /\ 29)
  : (53 /\ 29)
  : (61 /\ 53)
  : (97 /\ 53)
  : (61 /\ 29)
  : (47 /\ 13)
  : (75 /\ 47)
  : (97 /\ 75)
  : (47 /\ 61)
  : (75 /\ 61)
  : (47 /\ 29)
  : (75 /\ 13)
  : (53 /\ 13)
  : Nil

ruleMap :: Map Page Pages
ruleMap =
  Map.fromFoldable $
    (13 /\ (53 : 75 : 47 : 29 : 61 : 97 : Nil))
    : (29 /\ (47 : 61 : 53 : 97 : 75 : Nil))
    : (47 /\ (75 : 97 : Nil))
    : (53 /\ (97 : 61 : 75 : 47 : Nil))
    : (61 /\ (75 : 47 : 97 : Nil))
    : (75 /\ (97 : Nil))
    : Nil

