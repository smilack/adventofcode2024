module AdventOfCode.Twenty24.One
  ( distances
  , main
  , pairUp
  , parse
  , solve1
  , sort
  ) where

import Prelude

import AdventOfCode.Twenty24.One.Pair (Pair(..))
import Data.Either (Either, fromRight)
import Data.List (List(..))
import Data.List as List
import Data.List.Lazy as Lazy
import Data.List.ZipList (ZipList(..))
import Data.Newtype (unwrap)
import Data.Ord (abs)
import Data.Traversable (sequence, sum)
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

-- log ""
-- logShow $ solve2 input

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

