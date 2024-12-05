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
  , solve1
  , validManuals
  ) where

import AdventOfCode.Prelude

import AdventOfCode.Twenty24.Util (eol, multiline)
import Data.Foldable (elem, sum)
import Data.List (filter)
import Data.List as List
import Data.Map as Map
import Data.Maybe (fromMaybe)
import Data.Set (Set, member)
import Data.Set as Set
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
    logShow $ solve1 input
    log "Part 2:"
    -- logShow $ solve2 input
    log "End"

solve1 :: String -> Int
solve1 = sum <<< map (fromMaybe 0 <<< middle <<< un Manual) <<< validManuals

validManuals :: String -> List Manual
validManuals s = filter (checkManual rules) manuals
  where
  rules /\ manuals = parse s

checkManual :: Map Page Pages -> Manual -> Boolean
checkManual rules m = go pages m
  where
  pages = Set.fromFoldable $ unwrap m

  go _ (Manual Nil) = true
  go valid (Manual (p : ps))
    | p `member` valid = go (cutPages valid p) (Manual ps)
    | otherwise = false

  cutPages valid page =
    Set.difference valid $ fromMaybe Set.empty $ Map.lookup page rules

middle :: forall a. List a -> Maybe a
middle = go
  where
  go Nil = Nothing
  go (a : Nil) = Just a
  go (_ : _ : Nil) = Nothing
  go (_ : as) = go $ fromMaybe Nil $ List.init as

type Page = Int

type Pages = Set Page

newtype Manual = Manual (List Page)

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
combine = Map.fromFoldableWith (<>) <<< map (map Set.singleton <<< swap)

rule :: Parser String Rule
rule = do
  p1 <- intDecimal
  _ <- string "|"
  p2 <- intDecimal
  _ <- eol
  pure $ p1 /\ p2

manual :: Parser String Manual
manual = (Manual <<< List.fromFoldable) <$> (intDecimal `sepBy1` string ",")
