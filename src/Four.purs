module AdventOfCode.Twenty24.Four
  ( Point(..)
  , main
  , parse
  , solve1
  , solve2
  ) where

import AdventOfCode.Twenty24.Util (sumMap)
import Prelude

import Control.Alt ((<|>))
import Data.Either (fromRight)
import Data.FoldableWithIndex (foldrWithIndex)
import Data.Generic.Rep (class Generic)
import Data.List (List(..), elem, (:))
import Data.Map (Map, lookup)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Show.Generic (genericShow)
import Data.Traversable (for, traverse)
import Data.Tuple (Tuple)
import Data.Tuple.Nested ((/\))
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

main :: Effect Unit
main = launchAff_ do
  input <- readTextFile UTF8 "./input/4"
  liftEffect do
    log "Part 1:"
    logShow $ solve1 input
    log "Part 2:"
    logShow $ solve2 input

solve1 :: String -> Int
solve1 = checkGrid checkX <<< parse

solve2 :: String -> Int
solve2 = checkGrid checkA <<< parse

checkGrid :: CheckFn -> Map Point Char -> Int
checkGrid cf m = foldrWithIndex (cf m) 0 m

type CheckFn = Map Point Char -> Point -> Char -> Int -> Int

checkX :: Map Point Char -> Point -> Char -> Int -> Int
checkX m p c acc = acc + case c of
  'X' -> chexmas p m
  _ -> 0

chexmas :: Point -> Map Point Char -> Int
chexmas p m = sumMap check directions
  where
  check d = case traverse (_ `lookup` m) (d <@> p) of
    Just ('M' : 'A' : 'S' : Nil) -> 1
    _ -> 0

parse :: String -> Map Point Char
parse str = Map.fromFoldable $ fromRight Nil $ runParser str grid

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

-- part 2

tlbr :: Direction
tlbr = shift (-1) (-1) : shift 1 1 : Nil

bltr :: Direction
bltr = shift (-1) 1 : shift 1 (-1) : Nil

mases :: List (List Char)
mases = ('M' : 'S' : Nil) : ('S' : 'M' : Nil) : Nil

isXmas :: Point -> Map Point Char -> Int
isXmas p m = if solidus && reverseSolidus then 1 else 0
  where
  diagonal shifts = for (flap shifts p) (_ `lookup` m)
  solidus = isMas $ diagonal bltr
  reverseSolidus = isMas $ diagonal tlbr
  isMas = case _ of
    Just chars -> elem chars mases
    Nothing -> false

checkA :: Map Point Char -> Point -> Char -> Int -> Int
checkA m p c acc = acc + case c of
  'A' -> isXmas p m
  _ -> 0