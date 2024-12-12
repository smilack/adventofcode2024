module AdventOfCode.Twenty24.Six
  ( LabCell(..)
  , main
  , toMap
  ) where

import AdventOfCode.Prelude

import AdventOfCode.Twenty24.Util (multiline, oneOfChar)
import AdventOfCode.Util.Area (Area)
import AdventOfCode.Util.Area as Area
import AdventOfCode.Util.Coord (Coord(..), Direction(..))
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Class (liftEffect)
import Effect.Console (log, logShow)
import Node.Encoding (Encoding(..))
import Node.FS.Aff (readTextFile)
import Parsing (Parser)

main :: Effect Unit
main = launchAff_ do
  input <- readTextFile UTF8 "./input/6"
  liftEffect do
    log "Part 1:"
    -- logShow $ solve1 input
    log "Part 2:"
    -- logShow $ solve2 input
    log "End"

-- solve1 :: String -> List (List LabCell)
-- solve1 = parseLab

turn :: Direction -> Direction
turn = case _ of
  Up -> Right
  Right -> Down
  Down -> Left
  Left -> Up

data LabCell
  = Empty
  | Obstructed
  | Visited
  | Guard Direction

instance Show LabCell where
  show = case _ of
    Empty -> "░"
    Obstructed -> "█"
    Visited -> " "
    Guard d -> show d

labCellCharacters :: Array (Char /\ LabCell)
labCellCharacters =
  [ ('.' /\ Empty)
  , ('#' /\ Obstructed)
  , ('^' /\ Guard Up)
  , ('>' /\ Guard Down)
  , ('v' /\ Guard Right)
  , ('<' /\ Guard Left)
  ]

toMap :: String -> Area LabCell
toMap = Area.fromFoldable <<< parseLab

parseLab :: String -> List (List LabCell)
parseLab s = fold $ runParser s lab

lab :: Parser String (List (List LabCell))
lab = multiline row

row :: Parser String (List LabCell)
row = many $ oneOfChar labCellCharacters
