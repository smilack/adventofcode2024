module AdventOfCode.Twenty24.One
  ( distances
  , main
  , pairUp
  , parse
  , solve1
  , solve2
  , sort
  ) where

import Prelude

import AdventOfCode.Twenty24.One.Pair (Pair(..))
import AdventOfCode.Twenty24.Util (lookupWithDefault, sumMap, tally)
import Data.Either (Either, fromRight)
import Data.List (List(..))
import Data.List as List
import Data.List.Lazy as Lazy
import Data.List.ZipList (ZipList(..))
import Data.Map (Map)
import Data.Newtype (unwrap)
import Data.Ord (abs)
import Data.Traversable (sequence, sum)
import Data.Tuple (Tuple, uncurry)
import Data.Tuple.Nested ((/\))
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Class (liftEffect)
import Effect.Console (log, logShow)
import Node.Encoding (Encoding(..))
import Node.FS.Aff (readTextFile)
import Parsing (Parser, runParser)
import Parsing.Combinators (many)
import Parsing.String.Basic (intDecimal, skipSpaces)

main :: Effect Unit
main = launchAff_ do
  input <- readTextFile UTF8 "./input/1"
  liftEffect do
    log "Part 1:"
    logShow $ solve1 input
    log "Part 2:"
    logShow $ solve2 input

-- Part 1

solve1 :: String -> Int
solve1 = sum <<< distances <<< pairUp <<< sort <<< parse

sort :: Pair (List Int) -> Pair (List Int)
sort = map List.sort

pairUp :: forall a. Pair (List a) -> List (Pair a)
pairUp =
  List.fromFoldable
    <<< unwrap -- Lazy.List (Pair a)
    <<< sequence -- :: ZipList (Pair a))
    <<< map ZipList -- :: Pair (ZipList Int)
    <<< map Lazy.fromFoldable -- :: Pair (Lazy.List Int)

distances :: forall a f. Ord a => Ring a => Functor f => f (Pair a) -> f a
distances = map distance
  where
  distance (Pair { a, b }) = abs $ a - b

-- Shared

parse :: String -> Pair (List Int)
parse s = toPairedLists $ runParser s readLines
  where
  toPairedLists :: forall a. Either a (List (Pair Int)) -> Pair (List Int)
  toPairedLists = sequence <<< fromRight Nil

readLines :: Parser String (List (Pair Int))
readLines = many readLine

readLine :: Parser String (Pair Int)
readLine = do
  a <- intDecimal
  skipSpaces
  b <- intDecimal
  skipSpaces
  pure $ Pair { a, b }

-- Part 2

solve2 :: String -> Int
solve2 s = sumMap score $ massLookupWithDefault 0 a $ tally b
  where
  Pair { a, b } = parse s

  score :: Tuple Int Int -> Int
  score = uncurry (*)

massLookupWithDefault
  :: forall f k v. Functor f => Ord k => v -> f k -> Map k v -> f (Tuple k v)
massLookupWithDefault v f m = map get f
  where
  get k = k /\ lookupWithDefault v k m
