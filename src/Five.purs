module AdventOfCode.Twenty24.Five
  ( Manual(..)
  , Page
  , Pages
  , Rule
  , combine
  , main
  , manual
  , middle
  , puzzleInput
  , rule
  ) where

import AdventOfCode.Prelude

import AdventOfCode.Twenty24.Util (eol, multiline)
import Data.List as List
import Data.Map as Map
import Data.Maybe (fromMaybe)
import Data.Tuple (swap)
import Node.Encoding (Encoding(..))
import Node.FS.Aff (readTextFile)
import Parsing (Parser)
import Parsing.Combinators (sepBy, sepBy1, sepEndBy, sepEndBy1)
import Parsing.String (string)
import Parsing.String.Basic (intDecimal, skipSpaces)

main :: Effect Unit
main = launchAff_ do
  input <- readTextFile UTF8 "./input/5"
  liftEffect do
    log "Part 1:"
    -- logShow $ solve1 input
    log "Part 2:"
    -- logShow $ solve2 input
    log "End"

-- solve1 :: 

middle :: forall a. List a -> Maybe a
middle = go
  where
  go Nil = Nothing
  go (a : Nil) = Just a
  go (_ : _ : Nil) = Nothing
  go (_ : as) = go $ fromMaybe Nil $ List.init as

type Page = Int

type Pages = List Page

newtype Manual = Manual Pages

derive instance Newtype Manual _
derive instance Eq Manual
instance Show Manual where
  show (Manual m) = show m

type Rule = Page /\ Page

parse :: String -> Map Page Pages /\ List Manual
parse = fromRight default <<< (_ `runParser` puzzleInput)
  where
  default = Map.empty /\ Nil

puzzleInput :: Parser String (Map Page Pages /\ List Manual)
puzzleInput = do
  r <- many rule
  skipSpaces
  m <- multiline manual
  pure $ combine r /\ m

combine :: List Rule -> Map Page Pages
combine = Map.fromFoldableWith (<>) <<< map (map List.singleton <<< swap)

rule :: Parser String Rule
rule = do
  p1 <- intDecimal
  _ <- string "|"
  p2 <- intDecimal
  _ <- eol
  pure $ p1 /\ p2

manual :: Parser String Manual
manual = (Manual <<< List.fromFoldable) <$> (intDecimal `sepBy1` string ",")
