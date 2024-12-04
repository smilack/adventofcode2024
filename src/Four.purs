module AdventOfCode.Twenty24.Four
  ( Point(..)
  , main
  , parse
  , solve1
  ) where

import AdventOfCode.Twenty24.Util
import Prelude

import Control.Alt ((<|>))
import Data.Either (fromRight)
import Data.FoldableWithIndex (foldrWithIndex)
import Data.Generic.Rep (class Generic)
import Data.List (List(..), (:))
import Data.Map (Map, lookup)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Show.Generic (genericShow)
import Data.Traversable (sequence, traverse)
import Data.Tuple (Tuple)
import Data.Tuple.Nested (Tuple3, (/\))
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Class (liftEffect)
import Effect.Console (log, logShow)
import Node.Encoding (Encoding(..))
import Node.FS.Aff (readTextFile)
import Parsing (Parser, Position(..), position, runParser)
import Parsing.Combinators (many, try)
import Parsing.String (char)
import Parsing.String.Basic (skipSpaces)
import PointFree ((<..))

main :: Effect Unit
main = launchAff_ do
  input <- readTextFile UTF8 "./input/4"
  liftEffect do
    log "Part 1:"
    logShow $ solve1 input
    log "Part 2:"

-- log ""
-- logShow $ solve2 input

solve1 :: String -> Int
solve1 = checkGrid <<< parse

checkGrid :: Map Point Char -> Int
checkGrid m = foldrWithIndex (checkPoint m) 0 m

checkPoint :: Map Point Char -> Point -> Char -> Int -> Int
checkPoint m p c acc = acc + case c of
  'X' -> chexmas p m
  _ -> 0

chexmas :: Point -> Map Point Char -> Int
chexmas p m = sumMap check directions
  where
  check d = case traverse (_ `lookup` m) (d <@> p) of
    Just ('M' : 'A' : 'S' : Nil) -> 1
    _ -> 0

parse :: String -> Map Point Char
parse s = Map.fromFoldable $ fromRight Nil $ runParser s grid

grid :: Parser String (List (Tuple Point Char))
grid = try $ many do
  Position { column, line } <- position
  c <- xmas
  skipSpaces
  pure $ (Point { x: column - 1, y: line - 1 }) /\ c

xmas :: Parser String Char
xmas = char 'X' <|> char 'M' <|> char 'A' <|> char 'S'

data Point = Point { x :: Int, y :: Int }

derive instance Generic Point _
derive instance Eq Point

instance Show Point where
  show = genericShow

instance Ord Point where
  compare (Point a) (Point b) = case compare a.y b.y of
    LT -> LT
    GT -> GT
    EQ -> compare a.x b.x

type Direction = List (Point -> Point)

shift :: Int -> Int -> Point -> Point
shift dx dy (Point { x, y }) = Point { x: x + dx, y: y + dy }

e :: Direction
e = shift 1 0 : shift 2 0 : shift 3 0 : Nil

se :: Direction
se = shift 1 1 : shift 2 2 : shift 3 3 : Nil

s :: Direction
s = shift 0 1 : shift 0 2 : shift 0 3 : Nil

sw :: Direction
sw = shift (-1) 1 : shift (-2) 2 : shift (-3) 3 : Nil

w :: Direction
w = shift (-1) 0 : shift (-2) 0 : shift (-3) 0 : Nil

nw :: Direction
nw = shift (-1) (-1) : shift (-2) (-2) : shift (-3) (-3) : Nil

n :: Direction
n = shift 0 (-1) : shift 0 (-2) : shift 0 (-3) : Nil

ne :: Direction
ne = shift 1 (-1) : shift 2 (-2) : shift 3 (-3) : Nil

directions :: List Direction
directions = e : se : s : sw : w : nw : n : ne : Nil