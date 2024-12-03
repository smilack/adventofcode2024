module AdventOfCode.Twenty24.Two
  ( Safety(..)
  , check
  , countSafe
  , main
  , parse
  , solve1
  ) where

import AdventOfCode.Twenty24.Util
import Prelude

import Data.Either (fromRight)
import Data.Foldable (class Foldable)
import Data.Generic.Rep (class Generic)
import Data.List (List(..), (:))
import Data.List as List
import Data.List.NonEmpty (fromList)
import Data.List.NonEmpty as NE
import Data.List.Types (NonEmptyList)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Ord (abs)
import Data.Show.Generic (genericShow)
import Data.Traversable (traverse)
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Class (liftEffect)
import Effect.Console (log, logShow)
import Node.Encoding (Encoding(..))
import Node.FS.Aff (readTextFile)
import Parsing (Parser, runParser)
import Parsing.Combinators (endBy, many, many1, sepBy, sepBy1, sepEndBy, skipMany, skipMany1, (<|>))
import Parsing.String (string)
import Parsing.String.Basic (intDecimal, whiteSpace)
import PointFree ((<..))

main :: Effect Unit
main = launchAff_ do
  input <- readTextFile UTF8 "./input/2"
  liftEffect do
    log "Part 1:"
    logShow $ solve1 input
    log "Part2:"

-- log ""
-- logShow $ solve2 input

data Safety = Safe | Unsafe

derive instance Eq Safety
derive instance Ord Safety
derive instance Generic Safety _
instance Show Safety where
  show = genericShow

solve1 :: String -> Int
solve1 = countSafe <<< map check <<< parse

countSafe :: List Safety -> Int
countSafe = lookupWithDefault 0 Safe <<< tally

check :: NonEmptyList Int -> Safety
check nel = go first tail
  where
  { head: first, tail } = NE.uncons nel

  second = case List.uncons tail of
    Nothing -> first
    Just rest -> rest.head

  ord = compare first second

  diff = abs <.. (-)

  go :: Int -> List Int -> Safety
  go _ Nil = Safe
  go prev (next : rest) = case compare prev next of
    EQ -> Unsafe
    ord'
      | ord /= ord' -> Unsafe
      | diff prev next > 3 -> Unsafe
      | otherwise -> go next rest

parse :: String -> List (NonEmptyList Int)
parse s = fromRight Nil $ runParser s readReports

readReports :: Parser String (List (NonEmptyList Int))
readReports = readReport `sepEndBy` eol

readReport :: Parser String (NonEmptyList Int)
readReport = intDecimal `sepBy1` string " "

eol :: Parser String String
eol = string "\n" <|> string "\r\n"